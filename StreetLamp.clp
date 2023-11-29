;CÓDIGO HECHO POR: Grupo 3D: Juan Francisco López Quilis

;*****************************
;****  VARIABLES GLOBALES  ***
;*****************************

(defglobal ?*nodos* = 0)  


;*************************
;****  BASE DE HECHOS  ***
;*************************

(deffacts bh
    ;Información estática
    (grid 6 6)
    (almacen 1 5)
    (cesto_max 3)
    (bombillas 1 2 3)       ;Este hecho se utilizará para la regla de recoger bombillas en el almacén
    (basurero 6 6)

    ;Información dinámica
    ;Añadir cesto
    (robot 3 1 0 farola 1 1 bombilla 3 farola 5 2 bombilla 3 farola 2 3 bombilla 3 nivel 0)
)


;*************************
;****  BASE DE REGLAS  ***
;*************************

(defrule arriba
    ;LHS
    (grid ?gx ?gy)
    (robot ?x ?y ?bomb $?lista_farolas nivel ?niv)
    (profundidad_max ?pmax)

    ;Tests para:
    ;Que no se salga del mapa (que cambiará si es iz, der, arr, ab)
    (test (< ?y ?gy))
    ;Que no coincida el movimiento con la posicion de una farola
    (test (not(member$ (create$ farola ?x (+ ?y 1)) $?lista_farolas))) ;el not lleva tambien parentesis (es operador)
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot ?x (+ ?y 1) ?bomb $?lista_farolas nivel (+ ?niv 1)))
	;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))
)

(defrule abajo
    ;LHS
    (grid ?gx ?gy)
    (robot ?x ?y ?bomb $?lista_farolas nivel ?niv)
    (profundidad_max ?pmax)

    ;Tests para:
    ;Que no se salga del mapa (que cambiará si es iz, der, arr, ab)
    (test (> ?y 1))
    ;Que no coincida el movimiento con la posicion de una farola
    (test (not(member$ (create$ farola ?x (- ?y 1)) $?lista_farolas)))  ;Igual tengo q poner =
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot ?x (- ?y 1) ?bomb $?lista_farolas nivel (+ ?niv 1)))
	;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))
)

(defrule izquierda
    ;LHS
    (grid ?gx ?gy)
    (robot ?x ?y ?bomb $?lista_farolas nivel ?niv)
    (profundidad_max ?pmax)

    ;Tests para:
    ;Que no se salga del mapa (que cambiará si es iz, der, arr, ab)
    (test (> ?x 1))
    ;Que no coincida el movimiento con la posicion de una farola
    (test (not(member$ (create$ farola (- ?x 1) ?y) $?lista_farolas))) ;Igual tengo q poner =
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot (- ?x 1) ?y ?bomb $?lista_farolas nivel (+ ?niv 1)))
	;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))
)

(defrule derecha
    ;LHS
    (grid ?gx ?gy)
    (robot ?x ?y ?bomb $?lista_farolas nivel ?niv)
    (profundidad_max ?pmax)

    ;Tests para:
    ;Que no se salga del mapa (que cambiará si es iz, der, arr, ab)
    (test (< ?x ?gx))
    ;Que no coincida el movimiento con la posicion de una farola
    (test (not(member$ (create$ farola (+ ?x 1) ?y) $?lista_farolas)))  ;Igual tengo q poner =
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot (+ ?x 1) ?y ?bomb $?lista_farolas nivel (+ ?niv 1)))
	;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))
)

(defrule reparar
    ;LHS
    (profundidad_max ?pmax)
    (robot ?x ?y ?bomb $?resto1 farola ?fx ?fy bombilla ?fbomb $?resto2 nivel ?niv)

    ;Tests para:
    ;Que el robot esté en la posición adyacente a una farola
    (test (or (or (and (eq ?x ?fx) (eq (+ ?y 1) ?fy)) (and (eq ?x ?fx) (eq (- ?y 1) ?fy)))
			  (or (and (eq (+ ?x 1) ?fx) (eq ?y ?fy)) (and (eq (- ?x 1) ?fx) (eq ?y ?fy)))
	))
    ;Que el robot tenga igual o más bombillas de las que necesita reparar
    (test (>= ?bomb ?fbomb))
    (test (< ?niv ?pmax))
    =>
    (assert (robot ?x ?y (- ?bomb ?fbomb) $?resto1 $?resto2 nivel (+ ?niv 1))
    ;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1)))
)

;Cada vez que entre al almacen, saldrán 3 posibles robots como mucho (no puede salir el robot con menos bombillas de
;las que entró). Cada una de estas ramas se irá ejecutando y alguna obtendrá la solucion
(defrule recoger
    ;LHS
    (almacen ?ax ?ay)
    (cesto_max ?cmax) 
    (bombillas $?b1 ?b $?b2)
    (profundidad_max ?pmax)
    (robot ?x ?y ?bomb $?resto1 nivel ?niv)

    ;Tests para:
    ;Comprobar posicion robot = posicion almacen
    (test (and (eq ?x ?ax) (eq ?y ?ay)))
    ;Comprobar que el robot no tiene en su cesto el máximo de bombillas
    (test (<= (+ ?bomb ?b) ?cmax))
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot ?x ?y (+ ?bomb ?b) $?resto1 nivel (+ ?niv 1)))
    ;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))

)

(defrule basurero
    ;LHS
    (basurero ?bax ?bay)
    (profundidad_max ?pmax)
    (robot ?x ?y ?bomb $?resto1 nivel ?niv)

    ;Test que tiene bombillas
    (test (> ?bomb 0))  ;bomb es > que 0
    ;Test de que el robot se posiciona en basurero
    (test (and (eq ?x ?bax) (eq ?y ?bay)))
    ;Además, todas las reglas tendrán que comparar el nivel de profundidad con el max del usuario
    (test (< ?niv ?pmax))
    =>
    (assert (robot ?x ?y 0 $?resto1 nivel (+ ?niv 1)))
    ;Añadir contador de nodos: (bind ?*nodos* (+ *nodos* 1)) donde bind es unión
    (bind ?*nodos* (+ ?*nodos* 1))

)

(defrule objetivo
    (declare (salience 100))
    ?f <- (robot ?x ?y 0 nivel ?niv)

	=> 
	(printout t "Solucion encontrada en el nivel: " ?niv crlf)
    (printout t "Hecho en el que se ha conseguido el objetivo: " ?f crlf)
    (printout t "NUMERO DE NODOS EXPANDIDOS / REGLAS DISPARADAS " ?*nodos* crlf)
    
    (halt)
)


(defrule no_solucion
    (declare (salience -99))
    (robot ?x ?y ?b $?resto nivel ?niv)
    
=>
    (printout t "SOLUCION NO ENCONTRADA" crlf)
    (printout t "NUMERO DE NODOS EXPANDIDOS / REGLAS DISPARADAS " ?*nodos* crlf)
    
    (halt))		


(deffunction inicio ()
        (reset)
	(printout t "Profundidad Maxima:= " )
	(bind ?pmax (read))
	(printout t "Tipo de Busqueda " crlf "    1.- Anchura" crlf "    2.- Profundidad" crlf )
	(bind ?a (read))
	(if (= ?a 1)
	       then    (set-strategy breadth)
	       else   (set-strategy depth))
        (printout t " Ejecuta run para poner en marcha el programa " crlf)
	(assert (profundidad_max ?pmax))
	
)

;La solucion optima debe dar en anchura 19
;(robot 1 1 0 farola 4 3 bombilla 3 farola 3 5 bombilla 2 farola 5 5 bombilla 2 nivel 0)