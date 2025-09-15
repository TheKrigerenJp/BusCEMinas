#lang racket/gui
(require racket/class
         racket/string
         "logica_josepa.rkt")

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

;; --- Utilidad: crear un bitmap sólido (para “colorear” el botón) ---
(define (bitmap-color ancho alto color%)
  (define bm (make-object bitmap% ancho alto))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-brush (make-object brush% color% 'solid))
  (send dc set-pen   (make-object pen% color% 1 'solid))
  (send dc draw-rectangle 0 0 ancho alto)
  (send dc set-bitmap #f)
  bm)

;; color pastel (azul suave)
(define color-celda (make-object color% 173 216 230)) ; light blue
(define tam-celda 28) ; tamaño de cada “botón-celda” en px

;; Botón para abrir el diálogo de tamaño
(new button%
     [parent contenedor]
     [label "Iniciar partida"]
     [callback (lambda (_boton _evento) (abrir-dialogo-tamano))])

;; -----------------------------
;; Diálogo de tamaño de tablero
;; -----------------------------
(define (abrir-dialogo-tamano)
  (define dialogo (new dialog% [label "Tamaño del tablero"] [parent principal]))
  (define panel   (new vertical-panel% [parent dialogo] [spacing 8] [horiz-margin 12] [vert-margin 12]))  

  (new message% [parent panel] [label "Ingrese enteros entre 8 y 15:"])

  (define fila (new horizontal-panel% [parent panel] [spacing 8]))
  (define campo-ancho (new text-field% [parent fila] [label "Ancho:"] [init-value "8"]))
  (define campo-alto  (new text-field% [parent fila] [label "Alto:"]  [init-value "8"]))

  (define aviso (new message% [parent panel] [label ""]))

  (define (parseo-int s)
    (let ([n (string->number (string-trim s))])
      (and (exact-integer? n) n)))
  (define (tamano-valido? n) (and (exact-integer? n) (<= 8 n 15)))

  (define (aceptar)
    (define w (parseo-int (send campo-ancho get-value)))
    (define h (parseo-int (send campo-alto  get-value)))
    (if (and (tamano-valido? w) (tamano-valido? h))
        (begin (send dialogo show #f) (on-seleccion-tamano w h))
        (send aviso set-label "Ingrese enteros entre 8 y 15 para Ancho y Alto.")))

  (define fila-botones (new horizontal-panel% [parent panel] [spacing 8] [alignment '(right center)]))
  (new button% [parent fila-botones] [label "Cancelar"] [callback (lambda (_boton _evento) (send dialogo show #f))])
  (new button% [parent fila-botones] [label "Aceptar"]  [callback (lambda (_boton _evento) (aceptar))])

  (send dialogo center 'parent)
  (send dialogo show #t))

;; ------------------------------------------------------------
;; Render del tablero: filas sin spacing, celdas sin márgenes
;; y con etiqueta bitmap del color pastel elegido
;; ------------------------------------------------------------
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
           [spacing 0])) ; <- sin espacio horizontal entre celdas

    (for ([celda fila])
      (new button%
           [parent fila-panel]
           ;; etiqueta coloreada:
           [label imagen-celda]
           ;; quitar márgenes para que “toquen”
           [horiz-margin 0]
           [vert-margin 0]
           ;; fijar tamaño
           [min-width tam-celda]
           [min-height tam-celda]
           [stretchable-width #f]
           [stretchable-height #f]
           [callback (lambda (_boton _evento) (void))]))))

  

(define (on-seleccion-tamano ancho alto)
  (define matriz (crear-tablero ancho alto))
  (renderizar-tablero matriz))

(send principal show #t)





