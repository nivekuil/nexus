(ns com.nivekuil.nexus
  (:require [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.misc.macros :refer [full-symbol]]
            [clojure.spec.alpha :as s]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.connect.runner :as pcr]
            [com.wsscode.pathom3.connect.planner :as pcp]
            [com.wsscode.pathom3.plugin :as p.plugin]
            [clojure.set :as set]
            [com.nivekuil.nexus :as nx]
            [clojure.string :as str]))

(defonce resolvers
  #_"The currently loaded resolvers. This is populated by
 evaluating the `def` macro and also dynamically at resolve
 time by walking the namespaces of input keywords."
  (atom {}))

(defonce bodies
  #_"The body of the `def` macro, used to track cache state so if
the resolver body changes, the cache is invalidated and the resolver is
run again."
  (atom {}))

(defonce log? false)
(defn log [& forms]
  (when log? (println (str/join " " (conj forms "nexus:")))))

(defmacro def
  "Basically `pco/defresolver` but with some magic, like automatically
  registering resolvers into a global atom.  `::pco/output` is
  automatically inferred from the symbol name, so the body just needs
  to return the value and not a map."
  {:style/indent 2
   :arglists     '([name docstring? arglist options? & body])}
  [& args]
  (let [{:keys [name docstring arglist body] :as params}
        (-> (s/conform ::pco/defresolver-args args)
            (update :arglist pco/normalize-arglist))

        arglist' (s/unform ::pco/operation-args arglist)
        defdoc   (cond-> [] docstring (conj docstring))
        fqsym    (full-symbol name (str *ns*))
        kw       (keyword fqsym)]
    (log "def" kw)
    `(let [options#  ~(pco/params->resolver-options
                       (-> params
                           (assoc ::pco/op-name fqsym)
                           (assoc-in [:options ::pco/output] [kw])))
           resolver# (pco/resolver
                      '~fqsym
                      options#
                      (fn ~name ~arglist'
                        {~kw (do ~@body)}))]

       (swap! resolvers assoc ~kw resolver#)

       ;; TODO this is too precise, need to account for reader tags which are
       ;; evaled before this point. e.g. #(inc %) becomes some random syntax
       ;; but we want to treat it as being semantically equal
       (swap! bodies assoc ~kw '~body)

       (def ~name ~@defdoc resolver#))))

(defn halt! [sysenv]
  (when sysenv
    (doseq [kw (some-> sysenv ::halt-path deref)]
      (when-let [halt-thunk (some-> sysenv ::halt-thunks deref kw)]
        (log "halting:" kw "=>" (halt-thunk))))
    (reset! (::running sysenv) {})
    (reset! (::halt-path sysenv) ())))

(p.plugin/defplugin initializer
  {::pcr/wrap-resolve
   (fn [resolve]
     (fn [env input]

       (let [node        (::pcp/node env)
             kw          (keyword (::pco/op-name node))
             new-body    (get @bodies kw)
             update-env! (fn [res]
                           (swap! (::running env) merge res)
                           (swap! (::cache-keys env) assoc
                                  kw {:old-input input
                                      :old-body  new-body})
                           (swap! (::halt-thunks env) assoc
                                  kw (some-> (get-in env [::pci/index-resolvers (symbol kw) :config ::halt])
                                             (partial (get res kw)))))]
         (log "entering" kw)
         (if-let [cache-map (some-> env ::cache-keys deref kw)]
           (let [{:keys [old-input old-body]} cache-map
                 old-result                   (-> env ::running deref kw)]
             (if (and (= old-input input)
                      (= old-body new-body)
                      (::nx/cache? node (::nx/cache? env true)))
               (do (log "hit reset cache for" kw)
                   {kw old-result})
               (do
                 (log "stale cache for" kw)
                 (when-let [halt-fn (-> env ::halt-thunks deref kw)]
                   (halt-fn))
                 (doto (resolve env input) update-env!))))

           (let [res (resolve env input)]
             (update-env! res)
             (swap! (::halt-path env) conj kw)
             (log "leaving" kw)
             res)))))})

(defn- resolvers->nses
  "Get the namespaces of resolvers inputs"
  [resolvers]
  (into #{} (comp (map (comp ::pco/input :config val))
                  cat
                  (keep namespace)
                  (map symbol))
        resolvers))

(defn init
  "Initialize the system environment.  This is analogous to Pathom's
  `p.eql/process`, but it returns the env assoced with some atoms,
  including: `::halt-path`, a list storing the order the system will
  be halted in, and `::running`, a map from each initialized attribute
  to the value returned from its `nx/def`.

  `:env-transform` is a function from env to env, e.g. to add your own
  plugins or enable pathom-viz:

  ```clojure
  :env-transform (fn [env] (-> env
                               (p.plugin/register ...)
                               (pathom-viz/connect-env ...)))
  ```
  "
  [config targets & {:keys [env-transform]
                     :or   {env-transform identity}}]

  ;; recursively require namespaces of target dependencies
  (loop [nses (into #{} (comp (keep namespace)
                              (map symbol))
                    targets)]
    (locking clojure.lang.RT/REQUIRE_LOCK
      (doseq [ns nses]
        (log "requiring" ns)
        (try (require ns) (catch Exception e (log e)))))

    (when-let [new-things (not-empty (set/difference (resolvers->nses @resolvers) nses))]
      (recur (set/union new-things nses))))

  (log "requiring done, available: " (mapv key @resolvers))
  (let [env (-> (pci/register (mapv val @resolvers))
                (p.plugin/register initializer)
                (assoc ::halt-path (atom ())
                       ::halt-thunks (atom {})
                       ::cache-keys (atom {})
                       ::running (atom {}))
                env-transform)]
    (try (p.eql/process env config targets)
         (catch Exception e (halt! env) (throw e)))

    env))

(defn reset [env config targets]
  (let [newenv (-> env
                   (dissoc ::pci/index-resolvers)
                   (pci/register (mapv val @resolvers)))]
    #_#_(require 'clj-async-profiler.core)
    (clj-async-profiler.core/profile
     (p.eql/process newenv config targets))
    (p.eql/process newenv config targets)
    newenv))


;;;;;;;; repl
(defonce sysenv nil)

(defn go [config targets opts]
  (if sysenv
    (do
      (alter-var-root #'sysenv (fn [env] (reset env config targets)))
      :restarted)
    (do (alter-var-root #'sysenv (fn [_] (init config targets opts)))
        :started)))

(defn stop []
  (if sysenv
    (do (halt! sysenv)
        (alter-var-root #'sysenv (constantly nil))
        :stopped)
    :already-stopped))
