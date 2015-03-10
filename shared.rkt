#lang racket

(provide tty-style terminal
         irc-style irc)

(require scribble/base
         scribble/core)

(define tty-style
  (make-style "code-tty" empty))

(define irc-style
  (make-style "irc-quote" empty))
(define irc-channel-style
  (make-style "irc-channel" empty))

(define (terminal . content)
  (nested #:style tty-style (apply verbatim content)))

(define (irc channel . content)
  (list (nested #:style irc-style
                  (apply verbatim content))
          (elem #:style irc-channel-style
                channel)))
