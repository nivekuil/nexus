(ns example
  (:require [com.nivekuil.nexus :as nx]
            [ring.adapter.jetty :as jetty]))

(nx/def handler [{:keys [profile]}]
  {}
  (fn [request]
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body (case profile
             :dev "Hello dev"
             :prod "Hello prod")}))


(nx/def server [{:keys [::handler ::port]}]
  {::nx/halt #(.stop %) ;; destructor, stops the server created by `jetty/run-jetty`
   }
  (jetty/run-jetty handler {:port port :join? false}))

(defn -main [& args]
  (let [config {:profile :dev
                ::port 8080}
        targets [::server] ;; we only need to specify the components our
                           ;; application cares about at the highest level,
                           ;; i.e. not the handler
        ]
    ;; this stores the env in nx/sysenv for convenience.  To use your own env call `nx/init` directly
    (nx/go config targets {})))

(comment
  ;; run this, peek at localhost:8080
  (-main)

  ;; you can call this to tear down the whole system
  (nx/stop)

  ;; we can also peek into the sysenv manually
  (let [server (->> nx/sysenv ::nx/running deref ::server)]
    (.stop server)))
