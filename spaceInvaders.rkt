#lang racket
(require racket/gui)

#| Criando estruturas do Jogo |#

(struct mundo          (nave monstros tiros)     #:transparent)
(struct corpo          (x y tamanho)                #:transparent)
(struct monstro corpo (patrulha-x rapidez-x)        #:transparent)
(struct tiro  corpo   (velocity-x velocity-y)   #:transparent)
(struct nave  corpo    (morto?)                   #:transparent)
(define cutruca #F)


#| Criando Constantes do Jogo |#

(define largura 500)        ;Largura da tela
(define altura  600)        ;Altura da tela
(define tamanhoNave 15)     ;Tamanho da Nave
(define tamanhoTiro   3)    ;Tamanho do Tiro
(define tamanhoMonstro 14)  ;Tamanho dos Monstros
(define pontos 0) ;Define pontos iniciais

#| Criando personagens do Jogo |#

;Cria Nave  
(define (novaNave x y)
  (define morto? #f)
  (nave x y tamanhoNave morto?))

;Cria Monstro
(define (novoMonstro x y)
  (define patrulha-x 1)
  (define rapidez-x 1.5)
  (monstro x y tamanhoMonstro patrulha-x rapidez-x))

;Cria Tiro
(define (novoTiro x y vx vy)
  (tiro x y tamanhoTiro vx vy))

;Cria Mundo
(define (criarMundo)
  (mundo (criarNave) (criarMonstro) '()))

#| Criando personagens e suas posições iniciais no Jogo |#

;Cria nova Nave e sua posição inicial
(define (criarNave)
  (define x (/ largura 2.))
  (define y (- altura (* 3.3 tamanhoNave)))
  (novaNave x y))

;Cria novos Monstros e suas posição inicial
(define (criarMonstro)
  (for/list ([i 36])
    (define x (+ 20 (* 50 (remainder i 9)))) ; 8 colunas
    (define y (+ 10 (* 55 (remainder i 4)))) ; 3 linhas
    (novoMonstro x y)))



#| Criando Atualizações no Jogo |#

;Atualizando Nave
(define (atualizarNave w)
  (match-define (mundo p is bs) w)
  (match-define (nave x y tamanho d) p)
  (define morto? (or d (colisoes? p bs)))  
  (define moverNave
    (cond [d                   p]
          [(tecla? 'left)  (cond [(> x 1) (nave (- x 3.) y tamanho morto?)][else (nave x y tamanho morto?)])]
          [(tecla? 'right) (cond [(< x (- largura (+ tamanhoNave 25))) (nave (+ x 3.) y tamanho morto?)][else (nave x y tamanho morto?)])]
          [else               (nave x y tamanho morto?)]))
  (mundo moverNave is bs))



;Atualizando Monstro
(define (atualizarMonstro i)
  (match-define (monstro x y tamanho patrulha-x rapidez-x) i)
  ;Se o invasor está fora do limite de patrulha, invertemos a velocidade
  (define velocidade-x-fator (if (<= 0 patrulha-x 29) 1 -1))
  (define nova-rapidez-x  (* velocidade-x-fator rapidez-x))
  (define novo-x          (+        x nova-rapidez-x))
  (define novo-y          (+        y 1))
  (define nova-patrulha-x (+ patrulha-x nova-rapidez-x))
  (monstro novo-x novo-y tamanho nova-patrulha-x nova-rapidez-x))


;Atualizando Tiros
(define (atualizarTiro b)
  (match-define (tiro x y tamanho vx vy) b)
  (tiro (+ x vx) (+ y vy) tamanho vx vy))


#| Criando o disparo de tiros dos monstros |#

;Disparo dos Monstros
(define (dispararTiroMonstro w)
  (match-define (mundo p is bs) w)
  (define (monstroDisparouTiro i)
    (match-define (monstro x y tamanho _ _) i)
    ;de vez em quando, se não houver amigos abaixo, crie um marcador
    (if (and (> (random) 0.985)
             (not (monstroAbaixo? i is)))
        (tiro x (+ y tamanho 1) 3 (- (random) 0.5) 5.)
        #f))
  (define novosTiros (filter-map monstroDisparouTiro is))
  (mundo p is (append novosTiros bs)))

(define (monstroAbaixo? i is)
  (match-define (corpo x y tamanho) i)
  (for/or ([b is])
    (and (<= x (corpo-x b) (+ x tamanho))
         (> (corpo-y b) (+ y tamanho)))))


#| Criando o disparo de tiros da nave |#

;Um jogador vivo dispara quando o espaço é pressionado
(define (dispararTiroNave w)
  (match-define (mundo p is bs) w)
  (cond
    [(nave-morto? p) w] ;sem tiro, quando morto
    [(tecla? #\space) ;espaço => novo disparo
     (match-define (nave x y tamanho d) p)
     (define b (novoTiro (+ x (/ tamanho 2.)) y 0 -7))
     (mundo p is (cons b bs))]
    [else w]))


#| ATUALIZAÇÕES |#

(define (atualizar w)
  (reiniciar
   (removerCorposColidindo
    (dispararTiroNave
     (dispararTiroMonstro
      (atualizarNave
       (atualizarMonstros
        (atualizarTiros w))))))))

;Atualizar Monstros
(define (atualizarMonstros w)
  (define is (mundo-monstros w))
  (struct-copy mundo w [monstros (map atualizarMonstro is)]))

;Atualizar Tiros
(define (atualizarTiros w)
  (struct-copy mundo w [tiros (map atualizarTiro (mundo-tiros w))]))


;Reiniciar
(define (reiniciar w)
  (if (tecla? #\r)
      (criarMundo)
      w))

#| Colisão |#

;Colidindo
(define (colidindo? b1 b2)
  (match-define (corpo x1 y1 s1) b1)
  (match-define (corpo x2 y2 s2) b2)
  (not (or (eq? b1 b2)
           (< (+ x1 s1) x2) (> x1 (+ x2 s2))
           (< (+ y1 s1) y2) (> y1 (+ y2 s2)))))

;Colisões
(define (colisoes? x bs)
  (for/or ([b bs]) (colidindo? x b)))

;Dentro da tela
(define (dentroTela? b)
  (match-define (corpo x y tamanho) b)
  (and (< -40 x (+ largura  40))
       (< -40 y (+ altura 40))))


;Remover Corpos Colidindo
(define (removerCorposColidindo w)
  (match-define (mundo p is bs) w)
  (define (colisoesSemTiros? x)
    (not (colisoes? x bs)))
  (mundo p 
         (filter colisoesSemTiros? is)
         (filter dentroTela? ;remova os tiros não visíveis
                 (filter colisoesSemTiros? bs))))


#| DESENHANDO |#

;Desenhar mundo
(define (desenharMundo w dc)
  (match-define (mundo p is bs) w)
  (desenharCorpos (append (list p) is bs) dc))

(define (desenharCorpos bs dc)
  (for ([b bs])
    (match-define (corpo x y s) b)
    ;(define c (if (nave? b) (if (nave-morto? b) "red" "green") "black"))    
    (if (equal? (if (nave? b) (if (nave-morto? b) "red" "green") "black") "red")
       (send dc draw-bitmap (make-object bitmap% "game-over.jpg") 10 0)
        (cond
          [(= s 3) (send dc draw-bitmap (make-object bitmap% "tiro.png") x y)]
          [(= s 14) (send dc draw-bitmap (make-object bitmap% "monstro.png") x y)]
          [(= s 15) (send dc draw-bitmap (make-object bitmap% "nave.png") x y)]
          [else (send dc draw-bitmap (make-object bitmap% "nave.png") x y)]
          ))))


#| ESTADO DA GUI |#

;O mundo
(define oMundo (criarMundo))


;;; Teclado
; O estado do teclado é mantido em uma tabela de hash.
; Use as teclas? para descobrir, se uma tecla é pressionada ou não.

(define oTeclado (make-hasheq))
(define (tecla! k) (hash-set! oTeclado k #t))
(define (teclaUp! k)   (hash-set! oTeclado k #f))
(define (tecla? k) (hash-ref  oTeclado k #f))

;;; Tela de pintura
; Os principais eventos enviados para a tela atualizam as informações no teclado.
; Pintar eventos chama desenhar mundo. Para evitar a cintilação, suspendemos a lavagem
; enquanto o desenho começa.
(define game-canvas%
  (class canvas%
    (define/override (on-event e) ;eventos de mouse
      'ignore)
    (define/override (on-char e)  ;eventos teclado
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (when (eq? release 'press)  ;tecla?
        (tecla! key))
      (when (eq? key 'release)    ;teclaUp?
        (teclaUp! release)
        (when (eq? release #\space)
          (play-sound "shoot.mp3" #t))))
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (send dc clear)
      (desenharMundo oMundo dc)
      (send this resume-flush))
    (super-new)))


;Cria tela
(define frame  (new frame%  [label "Space Invaders"]))

;Texto de pontos
(define msg (new message% [parent frame]
                          [label "Pontos: "]))

(define canvas (new game-canvas%
                    [parent frame]
                    [min-width largura]
                    [min-height altura]))
(send frame show #t)


; Comece um temporizador. Cada vez que o timer dispara, o mundo é atualizado.
(define timer (new timer% 
                   [notify-callback 
                    (λ () 
                      (set! oMundo (atualizar oMundo))
                      (send canvas on-paint))]
                   [interval 100])) ; in milliseconds
(send timer start 20)
