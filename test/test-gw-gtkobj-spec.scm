
(define-module (test-gw-gtkobj-spec)
  :use-module (g-wrap))

;;(use-modules (g-wrap gw-runtime-spec))

(use-modules (g-wrap gtkobj))
(use-modules (g-wrap gw-gtk-spec))
(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "test-gw-gtkobj")))

  (gw:wrapset-set-guile-module! ws '(test-gw-gtkobj))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-gtk")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include <gtk/gtk.h>\n")))

  (gw:wrap-gtkobj ws "GtkWidget")
  (gw:wrap-gtkobj ws "GtkContainer")

  (gw:wrap-function
   ws
   'test-gw-gtkobj-gtk-vbox-new
   '<gw:GtkWidget*>
   "gtk_vbox_new"
   '((<gw:bool> homogeneous) (<gw:int> spacing))
   "Make a vbox via g-wrap.")

  (gw:wrap-function
   ws
   'test-gw-gtkobj-gtk-container-set-reallocate-redraws
   '<gw:void>
   "gtk_container_set_reallocate_redraws"
   '((<gw:GtkContainer*> container) (<gw:bool> needs-redraws))
   "Set container reallocate redraws via g-wrap.")

  #t)
