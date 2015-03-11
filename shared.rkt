#lang racket

(provide ~~
         horizontal-rule-style horizontal-separator
         blockquote-style blockquote
         tty-style terminal
         irc-style irc
         codeblock-style codeblock)

(require scribble/base
         scribble/core
         scribble/html-properties)

(define ~~ (string-append -~- -~-))

(define blockquote-style
  (make-style "blockquote" empty))

(define horizontal-rule-style
  (make-style "horizontal-separator"
              (list (alt-tag "hr"))))

(define tty-style
  (make-style "code-tty" empty))

(define codeblock-style
  (make-style "code-block" empty))

(define irc-style
  (make-style "irc-quote" empty))
(define irc-channel-style
  (make-style "irc-channel" empty))

(define (blockquote . content)
  (apply nested #:style blockquote-style content))

(define (horizontal-separator)
  (elem #:style horizontal-rule-style))

(define (terminal . content)
  (nested #:style tty-style (apply verbatim content)))

(define (codeblock . content)
  (nested #:style codeblock-style (apply verbatim content)))

(define (irc channel . content)
  (list (nested #:style irc-style
                  (apply verbatim content))
          (elem #:style irc-channel-style
                channel)))
