#lang racket
(provide crear-tablero
         parseo-int
         tamano-valido?)

;; ==============================
;; Crear tablero vacío
;; ==============================
;; Genera un tablero de ancho x alto
;; Cada celda inicializada con valor-inicial (default = 0)
(define (crear-tablero ancho alto [valor-inicial 0])
  (build-list alto
              (lambda (_fila)
                (build-list ancho
                            (lambda (_col)
                              valor-inicial)))))

;; ==============================
;; Conversión de string a entero
;; ==============================
(define (parseo-int s)
  (parseo-int-aux (string->number (string-trim s))))

(define (parseo-int-aux n)
  (and (exact-integer? n) n))

;; ==============================
;; Validación de tamaño
;; ==============================
;; Verifica que el valor sea un entero válido entre 8 y 15
(define (tamano-valido? n)
  (and n (<= 8 n 15)))
