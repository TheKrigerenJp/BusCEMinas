#lang racket
(provide crear-tablero
         parseo-int
         tamano-valido?)

;; Se necesita importar random para generar las minas aleatoriamente
(require racket/random)

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


;; ==============================
;; Crear el tablero 
;; ==============================
(define (crear_tablero filas columnas)
  (build-list filas  ; crea una lista con la cantidad de filas que se desean
              (lambda (_) (build-list columnas (lambda (_) 0))))) ; Cada fila es lista de ceros



(define (actualizar_tablero tablero fila columna valor)
  (list-update tablero fila
               (lambda (fila-lista)
                 (list-update fila-lista columna (lambda (_) valor)))))



;;---------------------------------------------------
;;  Función para generar minas aleatorias
;;    Recibe el número de minas, filas y columnas
;;    Devuelve el tablero con las minas
;;---------------------------------------------------
(define (poner_minas tablero num-minas filas columnas)
  (define (generar-posicion)
    (values (random filas) (random columnas))) ; genera fila y columna aleatoria

  (define (poner-una-mina t)
    (let-values ([(f c) (generar-posicion)])
      (if (equal? (list-ref (list-ref t f) c) "*")
          (poner-una-mina t)        ; Si ya hay mina, intenta otra posición
          (actualizar_tablero t f c "*"))))

  (if (= num-minas 0)
      tablero
      (poner_minas (poner-una-mina tablero) (- num-minas 1) filas columnas)))

;;---------------------------------------------------
;; Función para contar minas alrededor de una celda Aqui esta el error, pero no se donde xd 
;;---------------------------------------------------
(define (contar-minas tablero fila columna filas columnas)
  (define vecinos
    (for/list ([i (in-range (- fila 1) (+ fila 2))]
               [j (in-range (- columna 1) (+ columna 2))]
               #:when (and (>= i 0) (< i filas) (>= j 0) (< j columnas)
                           (not (and (= i fila) (= j columna))))) ; excluir la celda central
      (list i j)))
  
  (for/fold ([count 0]) ([v vecinos])
    (if (equal? (list-ref (list-ref tablero (first v)) (second v)) "*")
        (+ count 1)
        count)))
;;---------------------------------------------------
;;   Función para llenar el tablero con números me imagino que aqui puede ser el error tambien
;;---------------------------------------------------
(define (llenar-numeros tablero filas columnas)
  (for/fold ([t tablero]) ([i (in-range filas)])
    (for/fold ([t t]) ([j (in-range columnas)])
      (if (equal? (list-ref (list-ref t i) j) "*")
          t
          (actualizar_tablero t i j (contar-minas t i j filas columnas))))))

;;---------------------------------------------------
;; Función que muestra el tablero
;;---------------------------------------------------
(define (mostrar-tablero tablero)
  (for-each displayln tablero))

;;---------------------------------------------------
;; Crear tablero 8x8 con 6 minas 10% nivel facil
;;---------------------------------------------------
(define filas 8)
(define columnas 8)
(define num-minas 6)

(define tablero (crear-tablero filas columnas))
(define tablero-con-minas (poner_minas tablero num-minas filas columnas))
(define tablero-final (llenar-numeros tablero-con-minas filas columnas))

;; Mostrar el tablero final
(mostrar-tablero tablero-final)