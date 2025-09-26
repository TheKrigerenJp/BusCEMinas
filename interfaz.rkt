#lang racket
(require racket/gui
         "logica.rkt") ; usar: preparar-tablero, revelar-pos, visible?, valor-en, etc.

;; =========================================================
;; Estado del juego
;; =========================================================
(define *filas* 0)
(define *columnas* 0)
(define *tablero* '())   ; números/"*"
(define *mascara* '())   ; 0/1 visibles
(define *btns* #f)       ; vector de vectores de button%
(define board-panel #f)  ; panel que contendrá las filas de botones
(define *modo-banderas?* #f)   ; este activa o desactiva las banderas desde un botón
(define *check-banderas* #f)   ; crea el botón de banderas una vez

;; ==== Color pastel y helper seguro =====
(define pastel-azul (make-object color% 210 225 245))
(define (try-set-bg w color)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (send w set-background-color color)))

;; =========================================================
;; Ventana principal
;; =========================================================
(define frame
  (new frame%
       [label "Buscaminas - GUI"]
       [width  800]
       [height 800]))

(define root (new vertical-panel% [parent frame] [stretchable-height #t] [stretchable-width #t]))
(define barra (new horizontal-panel% [parent root] [stretchable-height #f] [spacing 8]))



(new button%
     [parent barra]
     [label "Nuevo juego"]
     [callback (lambda (_btn _evt) (abrir-configurar-y-generar!))])

;; Contenedor del tablero
(set! board-panel (new vertical-panel% [parent root] [spacing 0]
                       [stretchable-height #t] [stretchable-width #t]))

(send frame show #t)

;; =========================================================
;; Utilidades de UI
;; =========================================================
(define (clear-panel! p)
  (send p change-children (lambda (kids) '())))

;; =========================================================
;; Diálogo de configuración (tamaño + dificultad)
;; =========================================================
(define (solicitar-configuracion-tablero)
  (define dlg   (new dialog% [label "Configurar Buscaminas"]))
  (define panel (new vertical-panel% [parent dlg] [spacing 6] [border 10]))

  (new message% [parent panel] [label "Tamaño del tablero (filas x columnas)"])

  (define hp1 (new horizontal-panel% [parent panel] [spacing 6]))
  (new message% [parent hp1] [label "Filas:"])
  (define campo-filas (new text-field% [parent hp1] [label ""] [init-value "8"] [min-width 60]))
  (new message% [parent hp1] [label "Columnas:"])
  (define campo-cols  (new text-field% [parent hp1] [label ""] [init-value "8"] [min-width 60]))

  (new message% [parent panel] [label "Dificultad (porcentaje de minas)"])
  (define caja-dificultad
    (new radio-box% [parent panel]
         [label "Dificultad"]
         [choices (list "Fácil (10%)" "Media (15%)" "Difícil (20%)")]
         [style '(vertical)]
         [selection 0]))

  (define botones (new horizontal-panel% [parent panel] [spacing 8]))
  (define res #f)

  (define (cerrar con-aceptar?)
    (when con-aceptar?
      (define filas-num    (parseo-int (string-trim (send campo-filas get-value))))
      (define columnas-num (parseo-int (string-trim (send campo-cols  get-value))))
      (define sel (send caja-dificultad get-selection))
      (define dif-sym (cond [(= sel 0) 'facil]
                            [(= sel 1) 'media]
                            [else      'dificil]))
      (when (and (tamano-valido? filas-num)
                 (tamano-valido? columnas-num))
        (set! res (list filas-num columnas-num dif-sym))))
    (send dlg show #f))

  (new button% [parent botones] [label "Cancelar"] [callback (lambda (_btn _evt) (cerrar #f))])
  (new button% [parent botones] [label "Aceptar"]  [callback (lambda (_btn _evt) (cerrar #t))])

  (send dlg center)
  (send dlg show #t)
  res)

;; =========================================================
;; Construir el grid
;; =========================================================
(define (construir-grid! filas columnas)
  (clear-panel! board-panel)
  (set! *btns* (make-vector filas))
  (for ([ri (in-range filas)])
    (define fila-panel (new horizontal-panel% [parent board-panel] [spacing 0]
                            [stretchable-height #f] [stretchable-width #t]))
    
    (define fila-vec (make-vector columnas))
    (for ([rj (in-range columnas)])
      (define i0 ri)
      (define j0 rj)
      (define b (new button%
               [parent fila-panel]
               [label " "]
               [min-width 28] [min-height 28]
               [callback (lambda (_btn _evt)
                           (if *modo-banderas?*
                               (clickBand i0 j0)
                               (on-click-celda i0 j0)))]))
      (vector-set! fila-vec rj b))
    (vector-set! *btns* ri fila-vec)))

;; =========================================================
;; Refrescar celdas
;; =========================================================
(define (refrescar-grid!)
  (for ([ri (in-range *filas*)])
    (for ([rj (in-range *columnas*)])
      (refrescar-celda! ri rj))))

(define (refrescar-celda! ri rj)
  (define b (vector-ref (vector-ref *btns* ri) rj))
  (cond
    [(hayBand? *mascara* ri rj)
     (send b set-label "P")
     (send b enable #t)]
    [(visible? *mascara* ri rj)
     (refrescar-celda-visible! b ri rj)]
    [else
     (send b set-label " ")
     (send b enable #t)]))

(define (refrescar-celda-visible! b ri rj)
  (define v (valor-en *tablero* ri rj))
  (cond
    [(equal? v "*") (send b set-label "*")]
    [(and (number? v) (= v 0)) (send b set-label "")]
    [else (send b set-label (number->string v))])
  (send b enable #f))

;; =========================================================
;; Click izquierdo normal
;; =========================================================
(define (on-click-celda i j)
  (when (not (bloqueadaband? *mascara* i j)) ; <- no hace nada si hay bandera
    (define res (revelar-pos *tablero* *mascara* i j *filas* *columnas*))
    (define nueva-mascara (first res))
    (define golpeo? (second res))
    (set! *mascara* nueva-mascara)
    (refrescar-grid!)
    (when golpeo?
      (message-box "Fin del juego" "¡Pisaste una mina!" #f '(ok))
      ;; Revelar todo tras perder:
      (set! *mascara*
            (build-list *filas* (lambda (_) (build-list *columnas* (lambda (_) 1)))))
      (refrescar-grid!))
     ;; Verifica si ganó
    (when (and (not golpeo?) (ganaste? *tablero* *mascara* *filas* *columnas*))
      (message-box "¡Felicidades!" "¡Has ganado el juego!" #f '(ok))
      ;; Revela las minas del tablero una vez terminado el juego
      (set! *mascara*
            (build-list *filas* (lambda (_) (build-list *columnas* (lambda (_) 1)))))
      (refrescar-grid!))))
            
        
    


;;Actualiza en ij y refresca

(define (clickBand i j)
  (set! *mascara* (PQband *mascara* i j))
  (refrescar-grid!))

;; =========================================================
;; Iniciar nuevo juego
;; =========================================================
(define (abrir-configurar-y-generar!)
  (define res (solicitar-configuracion-tablero))
  (when res
    (define filas-num (first res))
    (define columnas-num (second res))
    (define dif-sym (third res))
    (define pack (preparar-tablero filas-num columnas-num dif-sym))
    (define nuevo-tablero (first pack))
    (define nueva-mascara (second pack))
    (set! *filas* filas-num)
    (set! *columnas* columnas-num)
    (set! *tablero* nuevo-tablero)
    (set! *mascara* nueva-mascara)
    ;Tablero en consola
    (printf "Tablero generado (~ax~a, dificultad ~a)\n" *filas* *columnas* dif-sym)
    (printf "SOLUCIÓN (completa):\n")
    (mostrar-tablero *tablero*)
    (newline)
    (construir-grid! *filas* *columnas*)
    (refrescar-grid!)
    
;; Pone el botón de banderas hasta que se abra el tablero, si lo tocamos cambia el modo banderas a T
    ; y con eso se pueden colocar
    (when (not *check-banderas*)
      (set! *check-banderas*
            (new check-box%
                 [parent barra]
                 [label "Pon banderas"]
                 [value #f]
                 [callback (lambda (cb evt)
                             (set! *modo-banderas?* (send cb get-value)))])))))

