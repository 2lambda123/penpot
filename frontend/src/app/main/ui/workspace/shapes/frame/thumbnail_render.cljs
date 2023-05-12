;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.shapes.frame.thumbnail-render
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.config :as cf]
   [app.main.data.workspace.thumbnails :as dwt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.shapes.frame :as frame]
   [app.util.dom :as dom]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [debug :refer [debug?]]
   [rumext.v2 :as mf]))

(defn- remove-image-loading
  "Remove the changes related to change a url for its embed value. This is necessary
  so we don't have to recalculate the thumbnail when the image loads."
  [value]
  (if (.isArray js/Array value)
    (->> value
         (remove (fn [change]
                   (or
                    (= "data-loading" (.-attributeName change))
                    (and (= "attributes" (.-type change))
                         (= "href" (.-attributeName change))
                         (str/starts-with? (.-oldValue change) "http"))))))
    [value]))

(defn- create-svg-blob-uri-from
  [fixed-width fixed-height rect node style-node]
  (let [{:keys [x y width height]} rect
        viewbox (dm/str x " " y " " width " " height)

        ;; This is way faster than creating a node 
        ;; through the DOM API
        svg-data
        (dm/fmt "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" viewBox=\"%\" width=\"%\" height=\"%\" fill=\"none\">% %</svg>"
                viewbox
                fixed-width
                fixed-height
                (if (some? style-node) (dom/node->xml style-node) "")
                (dom/node->xml node))

        ;; create SVG blob
        blob (js/Blob. #js [svg-data] #js {:type "image/svg+xml;charset=utf-8"})
        url  (dm/str (.createObjectURL js/URL blob) "#svg")]
    ;; returns the url and the node
    url))

(defn use-render-thumbnail
  "Hook that will create the thumbnail data"
  [page-id {:keys [id] :as shape} node-ref rendered? disable? force-render]

  (let [frame-image-ref (mf/use-ref nil)
        disable-ref?    (mf/use-var disable?)

        regenerate-thumbnail (mf/use-var false)

        all-children-ref (mf/use-memo (mf/deps id) #(refs/all-children-objects id))
        all-children     (mf/deref all-children-ref)

        {:keys [x y width height] :as shape-bb}
        (if (:show-content shape)
          (gsh/selection-rect (concat [shape] all-children))
          (-> shape :points gsh/points->selrect))

        [fixed-width fixed-height]
        (if (> width height)
          [(mth/clamp width 250 2000)
           (/ (* height (mth/clamp width 250 2000)) width)]
          [(/ (* width (mth/clamp height 250 2000)) height)
           (mth/clamp height 250 2000)])

        svg-uri      (mf/use-state nil)
        bitmap-uri   (mf/use-state nil)
        observer-ref (mf/use-var nil)
        shape-bb-ref (hooks/use-update-var shape-bb)
        updates-str  (mf/use-memo #(rx/subject))

        thumbnail-uri-ref (mf/use-memo (mf/deps page-id id) #(refs/thumbnail-frame-data page-id id))
        thumbnail-uri     (mf/deref thumbnail-uri-ref)

        ;; State to indicate to the parent that should render the frame
        render-frame? (mf/use-state (not thumbnail-uri))

        on-bitmap-load
        (mf/use-callback
         (fn []
           ;; We revoke the SVG Blob URI to free memory
           ;; only when we are sure that it is not used 
           ;; anymore.
           (.revokeObjectURL js/URL @svg-uri)
           (reset! svg-uri nil)))
        
        on-svg-load
        (mf/use-callback
         (fn []
           ;; FIXME: discriminate #svg blob uris from #bitmap uris
           (let [image-node (mf/ref-val frame-image-ref)]
             (dom/set-data! image-node "ready" "true")

             ;; If we don't have the thumbnail data saved (normally the first load) we update the data
             ;; when available
             (when (not @thumbnail-uri-ref)
               (st/emit! (dwt/update-thumbnail page-id id)))

             (reset! render-frame? false))))

        generate-thumbnail
        (mf/use-fn
         (fn generate-thumbnail []
           (try
             ;; When starting generating the canvas we mark it as not ready so its not send to back until
             ;; we have time to update it
             (let [node @node-ref]
               (if (dom/has-children? node)
                 ;; The frame-content need to have children in order to generate the thumbnail
                 (let [style-node (dom/query (dm/str "#frame-container-" (:id shape) " style"))
                       url  (create-svg-blob-uri-from fixed-width fixed-height @shape-bb-ref node style-node)]
                   (reset! svg-uri url))

                 ;; Node not yet ready, we schedule a new generation
                 (ts/schedule generate-thumbnail)))

             (catch :default e
               (.error js/console e)))))

        on-change-frame
        (mf/use-fn
         (fn []
           (when (and (some? @node-ref) @rendered? @regenerate-thumbnail)
             (let [loading-images? (some? (dom/query @node-ref "[data-loading='true']"))
                   loading-fonts? (some? (dom/query (dm/str "#frame-container-" (:id shape) " > style[data-loading='true']")))]
               (when (and (not loading-images?) (not loading-fonts?))
                 (generate-thumbnail)
                 (reset! regenerate-thumbnail false))))))

        ;; When the frame is updated, it is marked as not ready 
        ;; so that it is not sent to the background until 
        ;; it is regenerated.
        on-update-frame
        (mf/use-fn
         (fn []
           (let [image-node (mf/ref-val frame-image-ref)]
             (when (not= "false" (dom/get-data image-node "ready"))
               (dom/set-data! image-node "ready" "false")))
           (when (not @disable-ref?)
             (reset! render-frame? true)
             (reset! regenerate-thumbnail true))))

        on-load-frame-dom
        (mf/use-fn
         (fn [node]
           (when (and (some? node) (nil? @observer-ref))
             (when-not (some? @thumbnail-uri-ref)
               (rx/push! updates-str :update))

             (let [observer (js/MutationObserver. (partial rx/push! updates-str))]
               (.observe observer node #js {:childList true :attributes true :attributeOldValue true :characterData true :subtree true})
               (reset! observer-ref observer)))))]

    (mf/use-effect
     (mf/deps thumbnail-uri)
     (fn []
       (when (some? thumbnail-uri)
         (reset! bitmap-uri thumbnail-uri))))
    
    (mf/use-effect
     (mf/deps force-render)
     (fn []
       (when force-render
         (rx/push! updates-str :update))))

    (mf/use-effect
     (fn []
       (let [subid (->> updates-str
                        (rx/map remove-image-loading)
                        (rx/filter d/not-empty?)
                        (rx/catch (fn [err] (.error js/console err)))
                        (rx/subs on-update-frame))]
         #(rx/dispose! subid))))

    ;; on-change-frame will get every change in the frame
    (mf/use-effect
     (fn []
       (let [subid (->> updates-str
                        (rx/debounce 400)
                        (rx/observe-on :af)
                        (rx/catch (fn [err] (.error js/console err)))
                        (rx/subs on-change-frame))]
         #(rx/dispose! subid))))

    (mf/use-effect
     (mf/deps disable?)
     (fn []
       (when (and disable? (not @disable-ref?))
         (rx/push! updates-str :update))
       (reset! disable-ref? disable?)))

    (mf/use-effect
     (fn []
       #(when (and (some? @node-ref) @rendered?)
          (mf/unmount @node-ref)
          (reset! node-ref nil)
          (reset! rendered? false)
          (when (some? @observer-ref)
            (.disconnect @observer-ref)
            (reset! observer-ref nil)))))

    [on-load-frame-dom
     @render-frame?
     (mf/html
      [:& frame/frame-container {:bounds shape-bb
                                 :shape (cond-> shape
                                          (some? thumbnail-uri)
                                          (assoc :thumbnail thumbnail-uri))}

       ;; Safari don't support filters so instead we add a rectangle around the thumbnail
       (when (and (cf/check-browser? :safari) (debug? :thumbnails))
         [:rect {:x (+ x 2)
                 :y (+ y 2)
                 :width (- width 4)
                 :height (- height 4)
                 :stroke "blue"
                 :stroke-width 2}])

       ;; This is similar to how double-buffering works.
       ;; In svg-uri we keep the SVG image that is used to
       ;; render the bitmap until the bitmap is ready
       ;; to be rendered on screen. Then we remove the
       ;; svg and keep the bitmap one.
       ;; This is the "buffer" that keeps the bitmap image.
       (when (some? @bitmap-uri)
         [:image.thumbnail-bitmap
          {:x x
           :y y
           :width width
           :height height
           :href @bitmap-uri
           ;; DEBUG
           :style {:filter (when (debug? :thumbnails) "sepia(1)")}
           :on-load on-bitmap-load}])

       ;; This is the "buffer" that keeps the SVG image.
       (when (some? @svg-uri)
         [:image.thumbnail-canvas
          {:x x
           :y y
           :key (dm/str "thumbnail-canvas-" (:id shape))
           :data-object-id (dm/str page-id (:id shape))
           :width width
           :height height
           :ref frame-image-ref
           :href @svg-uri
           ;; DEBUG
           :style {:filter (when (debug? :thumbnails) "sepia(0.5)")}
           :on-load on-svg-load}])])]))
