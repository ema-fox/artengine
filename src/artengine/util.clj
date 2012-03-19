(ns artengine.util
  (:import [java.awt Color RenderingHints Polygon]
           [java.awt.image BufferedImage]
           [java.awt.event KeyListener KeyEvent MouseListener MouseMotionListener]
	   [java.awt.geom GeneralPath]
           [javax.swing JPanel JFrame JColorChooser]
           [javax.imageio ImageIO]))

(def tau (* 2 Math/PI))

(defn fill-rect [g [p0 p1] [s0 s1]]
  (.fillRect g p0 p1 s0 s1))

(defn draw-line [g [pa0 pa1] [pb0 pb1]]
  (.drawLine g pa0 pa1 pb0 pb1))

(defn minus [[pa0 pa1] [pb0 pb1]]
  [(- pa0 pb0) (- pa1 pb1)])

(defn plus [[pa0 pa1] [pb0 pb1]]
  [(+ pa0 pb0) (+ pa1 pb1)])

(defn mult [[p0 p1] x]
  [(* p0 x) (* p1 x)])

(defn mult2 [[pa0 pa1] [pb0 pb1]]
  [(* pa0 pb0) (* pa1 pb1)])

(defn direction [[pa0 pa1 :as pa] [pb0 pb1 :as pb]]
  (if (not= pa pb)
    (let [d0 (- pb0 pa0)
	  d1 (- pb1 pa1)
	  dist (Math/sqrt (+ (* d0 d0) (* d1 d1)))]
      [(/ d0 dist) (/ d1 dist)])
    [0 0]))
    

(defn distance [[pa0 pa1] [pb0 pb1]]
  (let [d0 (- pb0 pa0)
        d1 (- pb1 pa1)
        dist (Math/sqrt (+ (* d0 d0) (* d1 d1)))]
    dist))

(defn normalize-rect [[pa0 pa1] [pb0 pb1]]
  (let [[pd0 pe0] (sort [pa0 pb0])
	[pd1 pe1] (sort [pa1 pb1])]
    [[pd0 pd1]
     [pe0 pe1]]))

(defn contains [pa pb [pc0 pc1]]
  (let [[[pd0 pd1]
	 [pe0 pe1]] (normalize-rect pa pb)]
    (and (<= pd0 pc0 pe0)
	 (<= pd1 pc1 pe1))))

(defn avg-point [pa pb x]
  (plus (mult pa x) (mult pb (- 1 x))))

(defn p-on-line [pa pb pc]
  (let [delta (minus pb pa)
	t (/ (apply + (mult2 (minus pc pa) delta))
	     (apply + (mult2 delta delta)))]
    (avg-point pb pa t)))

(defn arc<-dir [[p0 p1]]
  (if (< 0 (Math/asin p0))
    (Math/acos p1)
    (- tau (Math/acos p1))))

(defn dir<-arc [a]
  [(Math/sin a) (Math/cos a)])

(defn avec<-dvec [p]
  [(arc<-dir (direction [0 0] p))
   (distance [0 0] p)])

(defn dvec<-avec [[a dist]]
  (mult (dir<-arc a) dist))

(defn pairs [xs]
  (map (fn [x y] [x y])
       xs
       (rest xs)))

(defn alter-in [r key f & args]
  (alter r assoc key (apply f (get @r key) args)))

(defn draw-lines [g ps]
  (doseq [[pa pb] (pairs ps)]
    (draw-line g pa pb)))

(defn shape-contains [shape [p0 p1]]
  (.contains shape p0 p1))

(defn set-color [g [c0 c1 c2]]
  (.setColor g (Color. c0 c1 c2)))

(defn draw-rect [g pa pb]
  (let [[[pd0 pd1]
	 [pe0 pe1]] (normalize-rect pa pb)]
    (.drawRect g pd0 pd1 (- pe0 pd0) (- pe1 pd1))))

(defn make-polygon [ps]
  (Polygon. (int-array (map first ps)) (int-array (map second ps)) (count ps)))

(defn draw-polygon [g ps]
  (.draw g (make-polygon ps)))

(defn fill-polygon [g ps]
  (.fill g (make-polygon ps)))

(defn get-pos [e]
  [(.getX e) (.getY e)])

(defn dbg [x]
  (prn x)
  x)

(defn start [renderfn key-pressed mouse-released mouse-pressed mouse-moved mouse-dragged]
  (let [panel (doto (proxy [JPanel] []
		      (paint [g] (renderfn g)))
		(.addMouseListener (proxy [MouseListener] []
				     (mouseClicked [e])
				     (mouseEntered [e])
				     (mouseExited [e])
				     (mousePressed [e] (mouse-pressed e))
				     (mouseReleased [e] (mouse-released e))))
		(.addMouseMotionListener (proxy [MouseMotionListener] []
					   (mouseDragged [e] (mouse-dragged e))
					   (mouseMoved [e] (mouse-moved e)))))
	frame (doto (new JFrame)
		(.add panel) 
		.pack 
		.show
		(.setFocusTraversalKeysEnabled false)
		(.addKeyListener (proxy [KeyListener] []
				   (keyPressed [e] (key-pressed e))
				   (keyTyped [e])
				   (keyReleased [e])))
		(.requestFocus))]
    (fn []
      (.repaint frame))))

(defn get-color []
  (let [color (JColorChooser/showDialog nil "" nil)]
    [(.getRed color) (.getGreen color) (.getBlue color)]))
