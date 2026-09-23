#|
Date: Sun, 20 Nov 1994 21:21:22 -0500
To: maeda@is.tokushima-u.ac.jp
From: straz (Steve Strassmann)
Subject: dialogs for assigning values
Cc: info-mcl
Sender: owner-info-mcl@cambridge.apple.com

  > From: maeda@is.tokushima-u.ac.jp
  > Date: Mon, 21 Nov 94 10:24:21 JST
  > 
  > I've been trying to develop a simple user interface using menus and
  > dialogs but got stuck with two little problems.  
  > 1) I want to be able to input text in a dialog an assign that input
  >    to a variable for future use.  To do so I tried the editable-text-
  >    dialog-item; it lets me type in text but I can't get to assign it
  >    to any kind of structure.  What should I do?
  > 2) I created a "Quit" button in a dialog box but I cannot make the 
  >     window disappear or the application quit after clicking the 
  >    button. What am I missing?

Here's some code to get you started. To try it out, just
type (make-instance 'my-dialog). It's a very quick and dirty
example.

To assign a value in a dialog item to a variable, the program 
has to notice that the value has changed. You might want to do
this every time the user types a key. I was lazy and have it
just notice when you click on "Quit". Some programs "notice"
by specializing change-key-handler (see the MCL manual).

Don't forget to provide a "cancel" button, which closes the
window without changing any of the values.

|#

;============================================================

(defparameter *foo* "Initial value of *foo*")

(defclass my-dialog (window)
  ()
  (:default-initargs :color-p t
    :view-size #@(250 180)
    :view-subviews
    (list (make-dialog-item 'editable-text-dialog-item
                            #@(5 5)            ; position
                            #@(200 40)         ; size
                            *foo*              ; text
                            nil                ; action
                            :view-nick-name :text)
          (make-dialog-item 'default-button-dialog-item
                            #@(180 130)        ; position
                            #@(50 18)          ; size
                            "Quit"             ; text
                            'press-quit-button     ; action
                            :view-nick-name :quit))))
                      
(defun press-quit-button (button)
  (let ((dialog (view-container button)))
    (setq *foo* (dialog-item-text (view-named :text dialog)))
    (window-close dialog)
    (format t "~%The value of *foo* is now ~S" *foo*)))


;  (make-instance 'my-dialog)