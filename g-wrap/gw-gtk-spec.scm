;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrap C pointers as guile-gtk Scheme side objects.
;;;

(define-module (g-wrap gw-gtk-spec)
  :use-module (g-wrap))

(use-modules (g-wrap gtkobj))
(export gw:wrap-gtkobj)

(let ((m (gw:new-wrapset "gw-gtk")))
  ;; Declaring a g-wrap wrapset allows us to insert this code into all
  ;; the clients, but unless we add any actual types or functions to
  ;; this wrapset, or something similar, this wrapset won't generate any
  ;; meaningful C code of its own.  Until that time, we're not going
  ;; to bother generating a useless libgw-gtkobj.

  (gw:wrapset-set-guile-module! m '(g-wrap gw-gtk))

  (for-each
   (lambda (obj-name)
     (gw:wrap-gtkobj m obj-name))
   '("GtkObject"
     "GtkWidget"
     "GtkContainer"
     "GtkFixed"
     "GtkTreeItem"
     "GtkTree"
     "GtkData"
     "GtkAdjustment"
     "GtkCList"
     "GtkBin"
     "GtkEventBox"
     "GtkWindow"
     "GtkHandleBox"
     "GtkBox"
     "GtkButtonBox"
     "GtkHButtonBox"
     "GtkVButtonBox"
     "GtkToolbar"
     "GtkTable"
     "GtkButton"
     "GtkToggleButton"
     "GtkCheckButton"
     "GtkRadioButton"
     "GtkMisc"
     "GtkArrow"
     "GtkLabel"
     "GtkLayout"
     "GtkTipsQuery"
     "GtkVBox"
     "GtkHBox"
     "GtkItem"
     "GtkItemFactory"
     "GtkCombo"
     "GtkStatusbar"
     "GtkGammaCurve"
     "GtkSeparator"
     "GtkHSeparator"
     "GtkVSeparator"
     "GtkFrame"
     "GtkAspectFrame"
     "GtkProgressBar"
     "GtkProgress"
     "GtkTooltips"
     "GtkMenuShell"
     "GtkMenuBar"
     "GtkMenu"
     "GtkMenuItem"
     "GtkTearoffMenuItem"
     "GtkAccelLabel"
     "GtkCheckMenuItem"
     "GtkRadioMenuItem"
     "GtkOptionMenu"
     "GtkPixmap"
     "GtkViewport"
     "GtkScrolledWindow"
     "GtkListItem"
     "GtkList"
     "GtkNotebook"
     "GtkEditable"
     "GtkEntry"
     "GtkSpinButton"
     "GtkText"
     "GtkAlignment"
     "GtkDrawingArea"
     "GtkCurve"
     "GtkPreview"
     "GtkFileSelection"
     "GtkFontSelectionDialog"
     "GtkColorSelectionDialog"
     "GtkColorSelection"
     "GtkRange"
     "GtkScale"
     "GtkHScale"
     "GtkVScale"
     "GtkScrollbar"
     "GtkHScrollbar"
     "GtkVScrollbar"
     "GtkRuler"
     "GtkHRuler"
     "GtkVRuler"
     "GtkDialog"
     "GtkInputDialog"
     "GtkInvisible"
     "GtkCalendar"
     "GtkPlug"
     "GtkPaned"
     "GtkHPaned"
     "GtkVPaned"))

  #t)
