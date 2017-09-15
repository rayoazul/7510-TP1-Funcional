(ns logical-interpreter)

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn comprobarHecho [hecho, basedatos]
(let [nueva (str hecho)]
(if (str/includes? basedatos nueva) true false)))


(defn comprobarHechos [[h & hs],basedatos]
	(if (= 0 (count hs)) (comprobarHecho h basedatos)  
		(and (comprobarHecho h basedatos) (comprobarHechos hs basedatos)))
)

(defn obtenerHechos [regla,basedatos]
 (def cadenaHechos (subs (str regla) (+(str/index-of (apply str regla) "-") 2) (str/index-of (apply str regla) ".")))
 (def hechos (str/split cadenaHechos #", "))
 (comprobarHechos hechos basedatos)
) 

(defn colocarArgumentos [[x & xs], [y & ys], cadena,basedatos]
  (let[cadena (str/replace cadena x y)]
  (if (> (count xs) 0) (colocarArgumentos xs ys cadena basedatos) (obtenerHechos cadena basedatos))) 
 )
 
(defn determinarArgumentos [regla,input,basedatos]
	(def separados (subs regla  (+ (str/index-of regla "(") 1)  (str/index-of regla ")") ))
	(let [argumentos (str/split separados  #",")]	
		(def separadosinput (subs input (+ (str/index-of (apply str input) "(") 1)  (str/index-of input ")") ))
		(let [argumentosinput (str/split (apply str separadosinput)  #",")]
		(colocarArgumentos argumentos argumentosinput regla basedatos)
		)
	)
)

(defn encontrarRegla [nombreRegla, input, [l & ls],basedatos] 	
	(if (and (str/starts-with? l nombreRegla) (str/includes? l ":-")) (determinarArgumentos l input basedatos) (encontrarRegla nombreRegla input ls basedatos))
)	

(defn evaluate-query [basedatos,consulta]
	(try(def nombreregla (subs consulta 0 (str/index-of consulta "(")))
	;; Veo si es un hecho (podria ser un hecho que no este).
		(if (comprobarHecho consulta basedatos) 
		true
			;;Sino veo si es una regla (else)
		(
			if (nil? (some (fn[l] (and (str/starts-with? l nombreregla) (str/includes? l ":-") )  ) (str/split-lines basedatos))) 
				false (encontrarRegla nombreregla consulta (str/split-lines basedatos) basedatos)
		)
				
		)
	(catch Exception e nil))
	)