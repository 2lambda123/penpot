;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.annotation-helpers
  (:require
   [app.common.data.macros :as dm]
   [app.common.logging :as log]
   [app.common.types.components-list :as ctkl]
   [app.main.refs :as refs]))

;; Change this to :info :debug or :trace to debug this module, or :warn to reset to default
(log/set-level! :warn)

(defn- get-libraries
  "Retrieve all libraries, including the local file."
  []
  (let [workspace-data (deref refs/workspace-data)
        {:keys [id] :as local} workspace-data]
    (-> (deref refs/workspace-libraries)
        (assoc id {:id id
                   :data local}))))

(defn get-main-annotation
  [shape]
  (let [libraries      (get-libraries)
        library        (dm/get-in libraries [(:component-file shape) :data])
        component      (ctkl/get-component library (:component-id shape) true)]
    (:annotation component)))

(defn get-main-annotation-viewer
  [shape]
  (let [viewer-data    (deref refs/viewer-data)
        viewer-file    (:file viewer-data)
        objects        (deref (refs/get-viewer-objects))
        parent         (get objects (:parent-id shape))
        component-id   (:component-id parent)
        component-file (:component-file parent)
        library        (:data
                        (if (= component-file (:id viewer-file))
                          viewer-file
                          (get (:libraries viewer-data) component-file)))
        component      (when (and component-id library)  (ctkl/get-component library component-id true))]
    (:annotation component)))
