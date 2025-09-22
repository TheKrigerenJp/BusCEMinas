#lang racket

(require racket/random) ; para random


;; =========================================================
;; Todo lo que mandamos a interfaz.rkt
;; =========================================================
(provide crear-tablero
         parseo-int
         tamano-valido?
         preparar-tablero
         revelar-pos
         en-rango?
         valor-en
         es-mina?
         crear-mascara
         visible?)

;; ==============================
;; Crear tablero (ancho x alto)
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
;; Crear el tablero por filas/columnas (lista de listas)
;; ==============================
(define (crear_tablero filas columnas)
  (build-list filas
              (lambda (_)
                (build-list columnas (lambda (_) 0)))))

;; ==============================
;; Actualizar una celda (i,j) con valor
;; ==============================
(define (actualizar_tablero tablero fila columna valor)
  (list-update tablero fila
               (lambda (fila-lista)
                 (list-update fila-lista columna (lambda (_) valor)))))

;; ========================================================
;; Generar minas aleatorias
;;   Recibe: tablero, num-minas, filas, columnas
;;   Devuelve: tablero con minas "*"
;; ========================================================
(define (poner_minas tablero num-minas filas columnas)
  ;; Coloca una sola mina en una posición libre; si cae en una ya ocupada, reintenta.
  (define (poner-una t)
    (call-with-values
      (lambda () (values (random filas) (random columnas)))
      (lambda (f c)
        (if (equal? (list-ref (list-ref t f) c) "*")
            (poner-una t) ; ya había mina: reintenta
            (actualizar_tablero t f c "*")))))

  ;; Repite k veces
  (define (poner k t)
    (if (= k 0)
        t
        (poner (- k 1) (poner-una t))))

  (poner num-minas tablero))


;; =================================
;; Contar minas alrededor de una celda
;; Recordar que hubo un cambio, aqui estaba el unico error hasta ahora reportado
;; ==================================
(define (contar-minas tablero fila columna filas columnas)
  (define vecinos
    (for*/list ([i (in-range (- fila 1) (+ fila 2))]
                [j (in-range (- columna 1) (+ columna 2))]
                #:when (and (>= i 0) (< i filas)
                            (>= j 0) (< j columnas)
                            (not (and (= i fila) (= j columna)))))
      (list i j)))
  (for/fold ([count 0]) ([v vecinos])
    (if (equal? (list-ref (list-ref tablero (first v)) (second v)) "*")
        (+ count 1)
        count)))

;; =========================================
;; Llenar el tablero con números
;; =========================================
(define (llenar-numeros tablero filas columnas)
  (for/fold ([t tablero]) ([i (in-range filas)])
    (for/fold ([t t]) ([j (in-range columnas)])
      (if (equal? (list-ref (list-ref t i) j) "*")
          t
          (actualizar_tablero t i j (contar-minas t i j filas columnas))))))

;; =============================================================================================================
;; Mostrar tablero en consola, por favor que en las siguientes versiones alguien tome esta función y la aplique
;; Asi cuando estamos con el profe es mas sencillo explicar
;; =============================================================================================================
(define (mostrar-tablero tablero)
  (for-each displayln tablero))

;; =========================================================
;; De aqui en adelante es las ultimas funciones creadas
;; Usadas para la logica del tablero final
;; =========================================================

;; Funcion que verifica si el indice (i,j) se encuentra dentro de los limites
;; del tablero
(define (en-rango? i j filas columnas)
  (and (>= i 0) (< i filas) (>= j 0) (< j columnas)))

;; Funcion que obtiene el valor guardado en la casilla tablero[i][j]
;; Entonces retorna * o numero dependiendo si es mina o no
(define (valor-en tablero i j)
  (list-ref (list-ref tablero i) j))

;; ============================================
;; Funcion que revisa si en esa casilla es mina
;; Es un booleano
;; ============================================
(define (es-mina? tablero i j)
  (equal? (valor-en tablero i j) "*"))

;; Funcion que crea una especie de mascara la cual es la que mostramos
;; con lo que habia en las casillas al ganar o perder (0 = oculta, 1 = visible)
(define (crear-mascara filas columnas)
  (build-list filas (lambda (_) (build-list columnas (lambda (_) 0)))))

;; Funcion que pregunta si la casilla ya esta marcada como visible en la "mascara"
;; Es un booleano
(define (visible? mascara i j)
  (= (list-ref (list-ref mascara i) j) 1))

;; Funcion que marca como visible u oculta la celda
;; Se marca como 0 o 1
;; Se hace uso de la función actualizar_tablero para irlo actualizando
(define (set-visible mascara i j val)
  (actualizar_tablero mascara i j val))

;; ===============================================================================
;; Funcion que genera una lista de casillas vecinas a [i,j]
;; Excluyendo el centro y respetando los límites del tablero
;; ===============================================================================
(define (vecinos-de i j filas columnas)
  (for*/list ([ii (in-range (- i 1) (+ i 2))]
              [jj (in-range (- j 1) (+ j 2))]
              #:when (and (en-rango? ii jj filas columnas)
                          (not (and (= ii i) (= jj j)))))
    (list ii jj)))

;; =====================================================================================
;; Esta funcion convierte lo ingresado por el usuario como dificultadad en numero para
;; hacer las multiplicaciones y generar la cantidad de minas
;; =====================================================================================
(define (dificultad_a_porcentaje dif)
  (cond [(eq? dif 'facil)   0.10]
        [(eq? dif 'media)   0.15]
        [(eq? dif 'dificil) 0.20]
        [else               0.10]))

;; =================================================================================
;; Funcion que se encarga de generar el tablero para hacer uso de el en la interfaz
;; Da todos los datos necesarios por decirlo asi, cantidad de minas, crea tablero,
;; coloca las minas y llena de números. Crea y devuelve tambien la mascara de
;; visibilidad inicial que empieza todo oculto obvio
;; =================================================================================
(define (preparar-tablero filas columnas dificultad-sym)
  (define filas*    (inexact->exact (floor (if (< filas 2) 2 filas))))
  (define columnas* (inexact->exact (floor (if (< columnas 2) 2 columnas))))
  (define total     (* filas* columnas*))
  (define porc      (dificultad_a_porcentaje dificultad-sym))
  (define n-minas   (inexact->exact (round (* total porc))))
  (define n-minas*  (if (< n-minas 1) 1 n-minas))

  (define base          (crear_tablero filas* columnas*))
  (define con-minas     (poner_minas base n-minas* filas* columnas*))
  (define tablero-final (llenar-numeros con-minas filas* columnas*))
  (define mascara       (crear-mascara filas* columnas*))
  (list tablero-final mascara))

;; Funcion que revela la celda (i,j), si era mina la vuelve visible y genera el "golpe de mina"
;; Si no era mina:
;; Si el valor era 0, expande recursivamente como flood fill
;; https://www-freecodecamp-org.translate.goog/news/flood-fill-algorithm-explained-with-examples/?_x_tr_sl=en&_x_tr_tl=es&_x_tr_hl=es&_x_tr_pto=tc
;; Si es mayor a cero solo revela esa celda  
(define (revelar-pos tablero mascara i j filas columnas)
  (if (es-mina? tablero i j)
      (list (set-visible mascara i j 1) #t)
      (list (expandir-cero tablero (set-visible mascara i j 1) i j filas columnas) #f)))

;; ============================================================
;; Funcion que si la casilla vale 0, genera una expansión recursiva
;; mostrando las casillas hasta encontrar numeros mayor a cero, uso de DFS
;; https://www-programiz-com.translate.goog/dsa/graph-dfs?_x_tr_sl=en&_x_tr_tl=es&_x_tr_hl=es&_x_tr_pto=tc
;; ============================================================
(define (expandir-cero tablero mascara i j filas columnas)
  (define val (valor-en tablero i j))
  (if (and (number? val) (= val 0))
      (revelar-vecinos-cero tablero mascara (vecinos-de i j filas columnas) filas columnas)
      mascara))

;; Procesa recursivamente la lista de vecinos, los revela y si es otro cero usa DFS otra vez
(define (revelar-vecinos-cero tablero mascara pares filas columnas)
  (if (null? pares)
      mascara
      (procesar-par-vecino tablero mascara (car pares) (cdr pares) filas columnas)))

;; Toma un par '(ii jj), si ya es visible lo salta pero si no, entonces llama a
;; procesar-no-visible para marcar y decidir si se expande
(define (procesar-par-vecino tablero mascara par resto filas columnas)
  (define ii (car par))
  (define jj (cadr par))
  (if (visible? mascara ii jj)
      ;; si ya era visible, seguimos con el resto
      (revelar-vecinos-cero tablero mascara resto filas columnas)
      ;; si no era visible, manejamos el caso "no visible"
      (procesar-no-visible tablero mascara ii jj resto filas columnas)))

;; Esta funcion marca como visible el vecino. Si su valor es 0, realiza DFS
;; y si era mayor a 0 entonces solo revela y continua 
(define (procesar-no-visible tablero mascara ii jj resto filas columnas)
  (define mascara1 (set-visible mascara ii jj 1))
  (define val (valor-en tablero ii jj))
  (if (and (number? val) (= val 0))
      ;; expandimos el cero y seguimos
      (revelar-vecinos-cero
       tablero
       (revelar-vecinos-cero
        tablero
        (expandir-cero tablero mascara1 ii jj filas columnas)
        (vecinos-de ii jj filas columnas) filas columnas)
       resto filas columnas)
      ;; número > 0
      (revelar-vecinos-cero tablero mascara1 resto filas columnas)))



