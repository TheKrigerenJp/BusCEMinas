#lang racket
(provide crear-tablero)

;; crear-tablero
;; Filas independientes (sin aliasing) usando build-list.
(define (crear-tablero ancho alto [valor-inicial 0])
  (build-list alto (lambda (_fila)
                     (build-list ancho (lambda (_col) valor-inicial)))))



