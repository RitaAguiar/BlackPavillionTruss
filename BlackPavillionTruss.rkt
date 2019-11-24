#lang racket

;DEFINIÇÕES

;;GLOBAIS

;;;GEOMETRIA TRELIÇA DE AÇO

(define raio-no-trelica 0.08) ;raio nó treliça
(define aresta-fixed-no-trelica 0.2) ;aresta do cubo nó fixo
(define raio-barra-trelica 0.03) ;raio tubo de aço
(define esp-tubo-aco 0.01) ;espessura do tubo de açp
(define area-tubo-aco (* pi  ;área do tubo de aço
                         (- (sqr raio-barra-trelica)
                            (sqr (- raio-barra-trelica
                                    esp-tubo-aco)))))
(define peso-esp-aco (* 76.5 1000)) ;peso específico do aço em N/m3
(define comp-barras 0) ;comprimento inicial barras
(define n-nos 0) ;número inicial nós
(define robot #f) ;robot ou cálculo

;;;GEOMETRIA LAJE DE BETÃO

(define esp-laje 0.5) ;espessura da laje de cobertura
(define peso-esp-betao (* 25 1000)) ;peso específico do betão em N/m3

;;;MÉTRICAS TRELIÇA

(define n-l 8)        ; 6  8 10 ;número de intervalos longitudinais entre apoios
(define n-t (* n-l 2)) ;12 16 20 ;número de intervalos transversais total

;;;NÓS FIXOS

(define a1 (xy 0 14.8))
(define a2 (xy 0 7.4))
(define a3 (x 0))
(define a4 (xy 7.2 14.8))
(define a5 (x 7.2))
(define a6 (xy 14.8 14.8))
(define a7 (x 14.8))
(define a8 (xy 22.4 14.8))
(define a9 (x 22.4))
(define a10 (xy 30 14.8))
(define a11 (x 30))
(define a12 (xy 36.4 14.8))
(define a13 (xy 36.4 7.4))
(define a14 (x 36.4))

(define posicoes-nos-fixos
  (make-parameter (list a1 a2 a3 a4 a5 a6 a7 a8 a9
        a10 a11 a12 a13 a14)))

(define distancia-critica-apoio (make-parameter 0.4))

;CÁLCULOS

(define area-laje (* (distance a1 a3) (distance a1 a12))) ;área laje trelica-museu-cidade
(printf "area-laje = ~a\n" area-laje)
(define v-laje (* area-laje esp-laje)) ;volume da laje
(define carga-laje (* peso-esp-betao v-laje)) ;carga da laje 7254KN

;FUNÇÕES

;;NÓS DAS BARRAS

(define (nos-entre-fechados p0 p1 n)
  (define u (/c (-c p1 p0) n))
  (map-division (lambda (i) (+c p0 (*c u i)))
                0 n n))

(define (malha-nos-int p0 p1 p2 p3 n-t n-l)
  (define u02 (/c (-c p2 p0) n-l))
  (define u13 (/c (-c p3 p1) n-l))
  (map-division (lambda (i)
                  (nos-entre-fechados
                   (+c p0 (*c u02 i)) (+c p1 (*c u13 i))
                   n-t))
                1 (- n-l 1) (- n-l 2)))

(define (cria-nos-barras-s n-t n-l)
  (append
   (list (nos-entre-fechados a1 a3 n-t))
   (malha-nos-int a1 a3 a4 a5 n-t n-l)
   (list (nos-entre-fechados a4 a5 n-t))
   (malha-nos-int a4 a5 a6 a7 n-t n-l)
   (list (nos-entre-fechados a6 a7 n-t))
   (malha-nos-int a6 a7 a8 a9 n-t n-l)
   (list (nos-entre-fechados a8 a9 n-t))
   (malha-nos-int a8 a9 a10 a11 n-t n-l)
   (list (nos-entre-fechados a10 a11 n-t))
   (malha-nos-int a10 a11 a12 a14 n-t n-l)
   (list (nos-entre-fechados a12 a14 n-t))))

;;MATRIZ PONTOS SUPERIORES

(define mat-barras-s
  (cria-nos-barras-s n-t n-l))

;;TRELIÇA MUSEU CIDADE

(define (trelica-museu-cidade tipo profundidade)
  (define mat-barras-i
    (cria-nos-barras-i mat-barras-s profundidade))
  (nos-trelica mat-barras-s mat-barras-i)
  (case tipo
    ((warren-verticals)
     (barras-trelica-warren-verticals mat-barras-s mat-barras-i))
    ((pratt)
     (barras-trelica-pratt mat-barras-s mat-barras-i))
    ((howe)
     (barras-trelica-howe mat-barras-s mat-barras-i))))

;;;NÓS

(define (menor-distancia p pts)
  (foldl min 1000000 (for/list ((p1 pts))
                       (distance p p1))))

(define (perto-apoio? p)
  (< (menor-distancia p (posicoes-nos-fixos))
     (distancia-critica-apoio)))

(define (nos-fila-trelica fila)
  (for/list ((no fila))
    (if (perto-apoio? no)
        (fixed-no-trelica no)
        (no-trelica no))))

(define (nos-trelica pontos-i pontos-s)
  (for/list ((a-i pontos-i)
             (a-s pontos-s))
    (nos-fila-trelica a-i)
    (nos-fila-trelica a-s)))

(define (cria-nos-barras-i mat-barras-s profundidade)
  (define (mod-z p)
    (+z p (- profundidade)))
  (define (mod-fila fila)
    (map mod-z 
         (cdr (drop-right fila 1))))
  (map mod-fila mat-barras-s))

;;;BARRAS

(define (barras-trelica ps qs)
  (for/list ((p ps) (q qs))
    (barra-trelica p q)))

;;;;WARREN WITH VERTICALS

(define (barras-trelica-warren-verticals pontos-i pontos-s)
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
      
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (impares a-i) (impares a-s))
    (barras-trelica (impares b-i) (impares b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (impares a-s) (pares (cdr a-i)))
    (barras-trelica (impares b-s) (pares (cdr b-i)))

    ;diagonais faciais inferiores longitudinais crescentes
    (barras-trelica (impares a-i) (pares b-i))
    (barras-trelica (pares b-i) (pares (cdr c-i)))
    ;diagonais faciais inferiores longitudinais decrescentes
    (barras-trelica (pares b-i) (pares (cdr a-i)))
    (barras-trelica (impares c-i) (pares b-i))
    ;diagonais faciais superiores longitudinais crescentes
    (barras-trelica (impares a-s) (pares b-s))
    (barras-trelica (pares b-s) (pares (cdr c-s)))
    ;diagonais faciais superiores longitudinais decrescentes
    (barras-trelica (impares c-s) (pares b-s))
    (barras-trelica (pares b-s) (pares (cdr a-s)))

    ;diagonais espaciais crescentes
    (barras-trelica (impares a-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr c-i)))
    ;diagonais espaciais decrescentes
    (barras-trelica (impares c-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr a-i)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (impares c-i) (impares c-s))
          (barras-trelica (impares c-s) (pares (cdr c-i))))
        ;recursão
        (barras-trelica-warren-verticals (cddr pontos-i) (cddr pontos-s)))
    ))

;;HOWE

(define (barras-trelica-howe pontos-i pontos-s)
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))

    ;diagonais iniciais
    (barra-trelica (car a-i) (car a-s))
    (barra-trelica (car b-i) (car b-s))
    (barra-trelica (car a-i) (car b-s))
    (barra-trelica (car c-i) (car b-s))
    (barra-trelica (car a-i) (cadr b-i))
    (barra-trelica (car c-i) (cadr b-i))
    ;diagonais finais
    (barra-trelica (last a-s) (last a-i))
    (barra-trelica (last b-s) (last b-i))
    (barra-trelica (last b-s) (last a-i))
    (barra-trelica (last b-s) (last c-i))
    (barra-trelica (list-ref b-i (- (length b-i)2)) (last a-i))
    (barra-trelica (list-ref b-i (- (length b-i)2)) (last c-i))

    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-1 a-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 b-i)))
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-2 a-i) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 b-s)))

    ;diagonais faciais inferiores longitudinais 1a metade
    (barras-trelica (cdr (metade-1 b-i)) (cddr (metade-1 a-i)))
    (barras-trelica (cdr (metade-1 b-i)) (cddr (metade-1 c-i)))
    ;diagonais faciais inferiores longitudinais 2a metade
    (barras-trelica (drop-right (metade-2 a-i) 1) (cdr (drop-right (metade-2 b-i) 1)))
    (barras-trelica (drop-right (metade-2 c-i) 1) (cdr (drop-right (metade-2 b-i) 1)))
    ;diagonais faciais superiores longitudinais 1a metade
    (barras-trelica (metade-1 b-s) (cdr (metade-1 a-s)))
    (barras-trelica (metade-1 b-s) (cdr (metade-1 c-s)))
    ;diagonais faciais superiores longitudinais 2a metade
    (barras-trelica (metade-2 a-s) (cdr (metade-2 b-s)))
    (barras-trelica (metade-2 c-s) (cdr (metade-2 b-s)))

    ;diagonais espaciais 1a metade
    (barras-trelica (metade-1 b-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 c-i)))
    ;diagonais espaciais 2a metade
    (barras-trelica (drop-right (metade-2 a-i) 1) (cdr (metade-2 b-s)))
    (barras-trelica (drop-right (metade-2 c-i) 1) (cdr (metade-2 b-s)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonal facial inicial
          (barra-trelica (car c-i) (car c-s))
          ;diagonais faciais intermédias
          (barras-trelica (metade-1 c-s) (cddr (metade-1 c-i)))
          (barras-trelica (metade-2 c-i) (cdr (metade-2 c-s)))
          ;diagonal facial inicial
          (barra-trelica (last c-s) (last c-i)))
        ;recursão
        (barras-trelica-howe (cddr pontos-i) (cddr pontos-s)))
    ))

;;PRATT

(define (barras-trelica-pratt pontos-i pontos-s)
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))

    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 a-s))
    (barras-trelica (metade-1 b-i) (metade-1 b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-2 a-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 b-i)))
    
    ;diagonais faciais inferiores longitudinais metade 1
    (barras-trelica (metade-1 a-i) (cdr (metade-1 b-i)))
    (barras-trelica (metade-1 c-i) (cdr (metade-1 b-i)))
    ;diagonais faciais inferiores longitudinais metade 2
    (barras-trelica (metade-2 b-i) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 c-i)))
    ;diagonais faciais superiores longitudinais metade 1
    (barras-trelica (metade-1 a-s) (cdr (metade-1 b-s)))
    (barras-trelica (metade-1 c-s) (cdr (metade-1 b-s)))
    ;diagonais faciais superiores longitudinais metade 2
    (barras-trelica (metade-2 b-s) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 c-s)))

    ;diagonais espaciais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 b-s))
    (barras-trelica (metade-1 c-i) (metade-1 b-s))
    ;diagonais espaciais decrescentes
    (barras-trelica (metade-2 b-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 c-i)))
    
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (metade-1 c-i) (metade-1 c-s))
          (barras-trelica (metade-2 c-s) (cdr (metade-2 c-i))))
        ;recursão
        (barras-trelica-pratt (cddr pontos-i) (cddr pontos-s)))
    ))

;;OUTROS

(define (impares lista)
  (if (null? lista)
      (list)
      (if (null? (cdr lista))
          (list (car lista))
          (cons (car lista) (impares (cddr lista))))))

(define (pares lista)
  (if (null? lista)
      (list)
      (impares (cdr lista))))

(define (metade-1 lista)
  (drop-right lista (/ (- (length lista) 1) 2)))

(define (metade-2 lista)
  (drop lista (/ (- (length lista) 1) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;RHINO/AUTOCAD
; 
; (require rosetta/autocad)
; 
; ;;LAYERS
; 
; (define node-layer (create-layer "Nodes"))
; (define fixed-node-layer (create-layer "Fixed Nodes"))
; (define bar-layer (create-layer "Bars"))
; 
; ;;MODELO ANALÍTICO OU MODELO GEOMÉTRICO
; 
; (define karamba? (make-parameter #f)) ; -> ALTERAR AQUI para alternar entre linhas/pontos e cilindros/esferas <-
; 
; (define (fixed-no-trelica p)
;   (define d (/ aresta-fixed-no-trelica 2))
;   (with-current-layer fixed-node-layer
;     (if (karamba?)
;         (begin
;           (point p)
;           (set! n-nos (+ n-nos 1))
;           (printf "n-nos = ~a\n" n-nos))
;         (sphere p raio-no-trelica)
;         ; (box (+xyz p (- d)(- d)(- d)) (+xyz p d d d))
; )))
; 
; (define (no-trelica p)
;   (with-current-layer node-layer
;     (if (karamba?)
;         (begin
;           (point p)
;           (set! n-nos (+ n-nos 1))
;           (printf "n-nos = ~a\n" n-nos))
;         (sphere p raio-no-trelica))))
; 
; (define (barra-trelica p0 p1)
;       (with-current-layer bar-layer
;         (if (karamba?)
;             (begin
;               (line p0 p1)
;               (set! comp-barras (+ comp-barras (distance p0 p1)))
;               (printf "comp-barras = ~a\n" comp-barras))
;             (cylinder p0 raio-barra-trelica p1))))
; 
; ;GERAÇÃO DE GEOMETRIA
; 
; ; (delete-all-shapes)
; 
; 
; ;;TRELIÇA MUSEU DA CIDADE
; 
; ; (define profundidade 1.00)
; ; 
; ; (trelica-museu-cidade tipo profundidade)
; 
; 
; ;CARGA
; 
; ; (define (calc-load carga-laje n-nos comp-barras)
; ;       (define carga-barras
; ;         (* area-tubo-aco
; ;            peso-esp-aco comp-barras))
; ;       (printf "carga-laje = ~a N\n" carga-laje)
; ;       (printf "carga-barras = ~a N\n" carga-barras)
; ;       (/ (+ (- carga-laje) (- carga-barras)) n-nos))
; ; 
; ; (define load (calc-load carga-laje n-nos comp-barras))
; ; (printf "load = ~a N\n" load)
; 
; 
; ;RENDERIZAÇÃO
; 
; (render-dir "D:\\Rita\\Escola\\Dissertation")
; (render-size 1920 1080)
; (view
;   (xyz -4.70745 7.07454 2.08565)
;   (xyz 129.793 116.988 -72.1536)
;   100.0)
; (render-view "CAADRIA-truss-2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ROBOT

(require rosetta/robot/backend) (define create-layer list) (define-syntax-rule (with-current-layer l body ...) (begin body ...)) (define point list)

(define fixed-truss-node-family (truss-node-family-element (default-truss-node-family)
                                                           #:support (create-node-support "SupportA" #:ux #t #:uy #t #:uz #t)))

(default-truss-bar-family (truss-bar-family-element (default-truss-bar-family)
                                                    #:material (list "ElasticIsotropic"  ;nome
                                                                     I_MT_STEEL          ;tipo
                                                                     "Steel"             ;nome do tipo
                                                                     "I'm really steel"  ;nuance
                                                                     210000000000.0      ;E
                                                                     0.3                 ;nu - 
                                                                     81000000000.0       ;G
                                                                     77010.0             ;sw
                                                                     1.2E-05             ;cte
                                                                     0.04                ;dumpcoef
                                                                     235000000.0         ;fy
                                                                     360000000.0)        ;Limit Tension resistance
                                                    #:section (list "Tube"
                                                                    "ElasticIsotropic"
                                                                    #f    ;wood?
                                                                    (list (list #f    ;solid?
                                                                                (* raio-barra-trelica 2)
                                                                                esp-tubo-aco)))))

;;MODELO BIM PARA O ROBOT

(define (no-trelica p)
  (if robot
      (truss-node p)
      (begin
        (set! n-nos (+ n-nos 1))
        ; (printf "n-nos = ~a\n" n-nos)
)))

(define (fixed-no-trelica p)
  (if robot
      (truss-node p fixed-truss-node-family)
      (begin
        (set! n-nos (+ n-nos 1))
        ; (printf "n-nos = ~a\n" n-nos)
)))

(define (barra-trelica p0 p1)
  (if robot
      (truss-bar p0 p1)
      (begin
        (set! comp-barras (+ comp-barras (distance p0 p1)))
        ; (printf "(distance p0 p1) = ~a comp-barra = ~a\n"
;                 (distance p0 p1) comp-barras)
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Falta: Terminação Dinâmica e Intermédia, Geração de 'testes' valores sem duplicados, Print do melhor, Export dos resultados

;ANÁLISE ROBOT

(require (prefix-in % rosetta/robot/robot-com))
(require (prefix-in base- racket/base))

;Constantes
(define factor 100)
(define caso 1)
;(define carga -5000)      ;5000N = 5KN per node
(define testes 5)         ;Número de profundidades aleatórias a gerar
(define n-grids 1)           ;Número de grids/samples para o Latin Hypercube Sampling, (- p-max p-min) deve ser divisivel por n-grids
(define p-min 1.20)       ;Profundidade Mínima
(define p-max 1.22)       ;Profundidade Máxima
(define casas-decimais 2) ;Para arredondar no random
(define ficheiro "tests.txt")

; Geração de um numero aleatorio entre min e max
(define (constrained-random min max)
  (+ min (* (- max min)(base-random))))

;Geração de profundidades aleatórias entre 0.10 e 1.00.
;Tenta gerar 'testes' valores, mas remove valores duplicados.
;Devolve os valores ordenados em ordem crescente
(define (full-random)
  (sort
   (remove-duplicates
    (for/list ((i testes))
      (string->number
       (real->decimal-string
        (constrained-random p-min p-max) casas-decimais)))) <))

;Geração de profundidades entre 'p-min' e 'p-max' utilizando Latin Hypercube Sampling.
;Gera um valor para cada grid, sendo o número de grids definido por 'n-grids'
(define (latin-hypercube-sampling)
  (let* ((delta (/ (- p-max p-min) n-grids))
         (curr-min (- p-min delta)))
    (sort (for/list ((i n-grids))
            (set! curr-min (+ curr-min delta))
            (string->number
             (real->decimal-string
              (constrained-random curr-min (+ curr-min delta)) casas-decimais))) <)))

(define (store-single-result n-l tipo p l d)
  (let ((s (list (format "~a ~a - ~a,~a,~a" tipo n-l p l d))))
    (display-lines-to-file s ficheiro #:exists 'append)
    d))

;Função que gera a trelica e devolve o UZ Maximum Displacement
(define (analise-museu-cidade tipo profundidades)
  (printf "profundidades = ~a\n" profundidades)
  (for/list ((profundidade profundidades))
    (printf "profundidade = ~a\n" profundidade)
    (define (calc-load carga-laje n-nos comp-barras)
      (define carga-barras
        (* area-tubo-aco
           peso-esp-aco comp-barras))
      (printf "carga-laje = ~a N\n" carga-laje)
      (printf "carga-barras = ~a N\n" carga-barras)
      (/ (+ (- carga-laje) (- carga-barras)) n-nos))
    (set! n-nos 0)
    (printf "n-nos-inicial = ~a\n" n-nos)
    (set! comp-barras 0)
    (printf "comp-barras-inicial = ~a\n" comp-barras)
    (set! robot #f)
    (trelica-museu-cidade tipo profundidade)
    (printf "n-nos-final = ~a\n" n-nos)
    (printf "comp-barras-final = ~a\n" comp-barras)
    (define load (calc-load carga-laje n-nos comp-barras))
    (printf "load = ~a N\n" load)
    (set! robot #t)
    (with-robot-analysis (results)
      (begin
        (delete-all-shapes)
        (trelica-museu-cidade tipo profundidade)
        (printf "load = ~a N\n" load))
      (vz load)
      (list
       tipo
       n-l
       profundidade
       load
       (store-single-result
         tipo n-l profundidade load
        (apply min
               (for/list ((node (in-hash-values (%added-nodes))))
                 (cz (v*r (%node-displacement-vector
                           results
                           (%truss-node-data-id node)
                           caso)
                          factor)))))))))

;;ANÁLISE

(define (optimal-truss tipo profundidades)
  (let ((results
         (analise-museu-cidade tipo profundidades)))
    (list results
          ; (argmin third (abs (+ 2.56 results)))

          (argmax fifth results))))

;;EXECUÇÃO ANÁLISE

; (define profundidades (full-random))

; (define profundidades (latin-hypercube-sampling))

(define profundidades '(1.00))

(optimal-truss #;'warren-verticals 'pratt profundidades)
