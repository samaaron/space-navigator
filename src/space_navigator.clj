(ns
    #^{:doc "A library with which to read values from a 3DConnexion SpaceNavigator 3D mouse"
       :author "Sam Aaron"}
  space-navigator
  (:import (net.java.games.input AbstractController AbstractComponent ControllerEnvironment)))

(defrecord SpaceNavigator       [name controller x-com y-com z-com rx-com ry-com rz-com l-btn-com r-btn-com])
(defrecord SpaceNavigatorValues [x y z rx ry rz l-btn r-btn])

(defn- find-controller
  "Find a HID controller that matches the supplied regexp name-matcher. If there is more than one match, the first is selected"
  [name-matcher]
  (let [default-env (ControllerEnvironment/getDefaultEnvironment)
        matcher     (fn [contr] re-find name-matcher (.toString contr))
        controller  (first (filter matcher (.getControllers default-env)))]
    (if-not controller (throw (Exception. (str "Could not find HID controller matching " name-matcher ". Is the device connected?"))))

    controller))

(defn- components
  "Fetch a list of components associated with a given controller"
  [controller]
  (let [component-list (.getComponents controller)]
    (reduce #(assoc %1 (.getName %2) %2) {} component-list)))

(defn- find-component
  "Find a controller's component based on the component's name"
  [controller component-name]
  (let [components (components controller)]
    (components component-name)))

(defn- read-component
  "Read the value of a given controller's component without polling the controller to refresh the values"
  [#^AbstractComponent component]
  (.getPollData component))

(defn- space-navigator-controller
  "Find a HID controller matching the name SpaceNavigator. Raises an exception if none could be found or if the components of the device found don't match the expected capibilities"
  []
  (let [controller      (find-controller #"SpaceNavigator")
        expected-comps  ["x" "y" "z" "rx" "ry" "rz" "0" "1"]
        components      (components controller)
        has-comp?       (fn [comp-name] (contains? components comp-name))]
    (if (some false? (map has-comp? expected-comps))
      (throw (Exception. (str "Controller didn't have the required components. Expected " expected-comps ", found: " (keys components)))))

    controller))

(defn space-navigator
  "Create a SpaceNavigator record representing an attached device. If more than one attached device matches the name SpaceNavigator, the first is chosen."
  []
  (let [controller (space-navigator-controller)
        name       (.getName controller)
        x-com      (find-component controller "x")
        y-com      (find-component controller "y")
        z-com      (find-component controller "z")
        rx-com     (find-component controller "rx")
        ry-com     (find-component controller "ry")
        rz-com     (find-component controller "rz")
        l-btn-com  (find-component controller "0")
        r-btn-com  (find-component controller "1")]
    (SpaceNavigator. name controller x-com y-com z-com rx-com ry-com rz-com l-btn-com r-btn-com)))

(defn read-vals
  "Read all of the values for a given SpaceNavigator record. Returns a SpaceNavigatorValues record mapping component name to the current value."
  [space-navigator]
  (let [#^AbstractController controller (:controller space-navigator)
        _                               (.poll controller)
        x     (read-component (:x-com     space-navigator))
        y     (read-component (:y-com     space-navigator))
        z     (read-component (:z-com     space-navigator))
        rx    (read-component (:rx-com    space-navigator))
        ry    (read-component (:ry-com    space-navigator))
        rz    (read-component (:rz-com    space-navigator))
        l-btn (read-component (:l-btn-com space-navigator))
        r-btn (read-component (:r-btn-com space-navigator))]
    (SpaceNavigatorValues. x y z rx ry rz l-btn r-btn)))
