#lang racket/gui

(define principal (new frame%
                   [label "BusCEMinas"]
                   [width 500]
                   [height 500]))

(define msg (new message%
                 [parent principal]
                 [label "Prueba para interfaz"]))

(send principal show #t)
