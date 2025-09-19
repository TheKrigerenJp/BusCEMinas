#lang racket/gui
(require racket/class
         racket/string
         "logica.rkt")

;; ==============================
;; Ventana principal
;; ==============================
(define nombre-app "Buscaminas")
(define principal (new frame% [label nombre-app]))

(define contenedor
  (new vertical-panel%
       [parent principal]
       [spacing 10]
       [alignment '(center center)]
       [horiz-margin 80]
       [vert-margin 80]))

(new message%
     [parent contenedor]
     [label "Pulsa \"Iniciar partida\" y elige el tamaño (8 a 15)."])

;; Panel persistente del tablero
(define area-tablero
  (new vertical-panel%
       [parent contenedor]
       [alignment '(center center)]
       [spacing 0])) ; <- sin espacio vertical entre filas

;; ==============================
;; Utilidad: crear un bitmap sólido
;; ==============================
(define (bitmap-color ancho alto color%)
  (define bm (make-object bitmap% ancho alto))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-brush (make-object brush% color% 'solid))
  (send dc set-pen   (make-object pen% color% 1 'solid))
  (send dc draw-rectangle 0 0 ancho alto)
  (send dc set-bitmap #f)
  bm)

;; Colores y tamaños
(define color-celda (make-object color% 173 216 230)) ; azul claro
(define tam-celda 28) ; px por celda

;; ==============================
;; Botón inicial
;; ==============================
(new button%
     [parent contenedor]
     [label "Iniciar partida"]
     [callback (lambda (_boton _evento) (POP_Tama_Difi))])

;; ==============================
;; Diálogo de tamaño de tablero
;; ==============================
(define (POP_Tama_Difi)
  (define dialogo (new dialog% [label "Tamaño del tablero"] [parent principal]))
  (define panel   (new vertical-panel% [parent dialogo] [spacing 8] [horiz-margin 12] [vert-margin 12]))  

  (new message% [parent panel] [label "Ingrese enteros entre 8 y 15:"])
  (new message% [parent panel] [label "Dificultad (porcentaje de minas)"])
  
  (define caja-dificultad ;la caja que solicita la dificultad
  (new radio-box%
       [parent panel]
       [label "Dificultad"] 
       [choices (list "Fácil (10%)" "Media (15%)" "Difícil (20%)")]
       [style '(vertical)]
       [selection 0]))
  (define fila (new horizontal-panel% [parent panel] [spacing 8]))
  (define campo-ancho (new text-field% [parent fila] [label "Ancho:"] [init-value "8"]))
  (define campo-alto  (new text-field% [parent fila] [label "Alto:"]  [init-value "8"]))

  ;; Botón aceptar con validación general y mensaje emergente
  (define (aceptar)
    (define w (parseo-int (send campo-ancho get-value)))
    (define h (parseo-int (send campo-alto  get-value)))

    (define sel (send caja-dificultad get-selection))
    (define dif-sym (cond [(= sel 0) 'facil]
                          [(= sel 1) 'media]
                          [else      'dificil]))

    (when (and (tamano-valido? w)
               (tamano-valido? h))
      (generar-tablero-consola w h dif-sym) ; imprime en consola usando la funcion hecha para probar el tablero
      (send dialogo show #f))

    (if (and (tamano-valido? w) (tamano-valido? h))
        (begin
          (send dialogo show #f)
          (on-seleccion-tamano w h))
        (message-box "Error"
                     "Entrada inválida: use enteros entre 8 y 15."
                     dialogo
                     '(ok stop))))

  ;; Botones aceptar/cancelar
  (define fila-botones (new horizontal-panel% [parent panel] [spacing 8] [alignment '(right center)]))
  (new button% [parent fila-botones] [label "Cancelar"] [callback (lambda (_boton _evento) (send dialogo show #f))])
  (new button% [parent fila-botones] [label "Aceptar"]  [callback (lambda (_boton _evento) (aceptar))])

  (send dialogo center 'parent)
  (send dialogo show #t))

;; ==============================
;; Render del tablero
;; ==============================
(define (renderizar-tablero matriz)
  ;; limpiar área
  (for ([hijo (send area-tablero get-children)])
    (send area-tablero delete-child hijo))

  (define ancho (if (null? matriz) 0 (length (first matriz))))
  (define alto  (length matriz))

  (new message% [parent area-tablero] [label (format "Tablero: ~a x ~a" ancho alto)])

  (define imagen-celda (bitmap-color tam-celda tam-celda color-celda))

  (for ([fila matriz])
    (define fila-panel
      (new horizontal-panel%
           [parent area-tablero]
           [spacing 0])) ; sin espacio horizontal entre celdas

    (for ([celda fila])
      (new button%
           [parent fila-panel]
           [label imagen-celda]      ; etiqueta coloreada
           [horiz-margin 0]
           [vert-margin 0]
           [min-width tam-celda]
           [min-height tam-celda]
           [stretchable-width #f]
           [stretchable-height #f]
           [callback (lambda (_boton _evento) (void))]))))

;; ==============================
;; Callback selección tamaño
;; ==============================
(define (on-seleccion-tamano ancho alto)
  (define matriz (crear-tablero ancho alto))
  (renderizar-tablero matriz))

(send principal show #t)



