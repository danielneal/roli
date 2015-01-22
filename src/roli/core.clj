(ns roli.core
  (:import [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info Receiver ShortMessage]
           [clojure.lang IFn])
  (:require [clojure.core.async :refer [chan put! go <! >! <!! >!! go-loop]]))

(defn all-midi-devices
  "Find all currently connected midi devices"
  []
  (for [^MidiDevice$Info device-info (MidiSystem/getMidiDeviceInfo)]
    (let [device ^MidiDevice (MidiSystem/getMidiDevice device-info)
          receivers (.getMaxTransmitters device)
          transmitters (.getMaxReceivers device)]
      {:name (.getName device-info)
       :description (.getDescription device-info)
       :device-info device-info
       :device device
       :direction
       (cond
        (and (neg? transmitters) (neg? receivers)) :bi
        (neg? transmitters) :out
        (neg? receivers) :in
        :else nil)})))

(defn find-named-device
  "Returns a map of midi in and out ports for
  the first device that matches the given name.
  Assumes device has only one input and output"
  [devices device-name]
  (let [matching-devices (filter #(= (:name %) device-name) devices)
        grouped (group-by :direction matching-devices)]
    {:input (first (:in grouped))
     :output (first (:out grouped))}))

(defn async-receiver
  "Create an implementation of the Receiver interface
  that will put! to a core async channel when an
  event is received"
  [chan]
  (reify Receiver
    (send [this msg timestamp]
          (put! chan "test")
          (comment (put! chan {:msg  msg
                               :cmd (.getCommand msg)
                               :channel (.getChannel msg)
                               :data1 (.getData1 msg)
                               :data2 (.getData2 msg)})))))

(defn in-device->chan
  [devices device-name]
  (let [ch (chan)
        receiver (async-receiver ch)
        device (:device (:input (find-named-device devices device-name)))]

    (when-not (.isOpen device) (.open device))
    (.setReceiver (.getTransmitter device) receiver)
    ch))

(def ch (in-device->chan (all-midi-devices) "Seaboard GRAND"))

(go-loop []
         (when-let [msg (<! ch)]
           (println msg)
           (recur)))

