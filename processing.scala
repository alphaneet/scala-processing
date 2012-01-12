package com.github.alphaneet.scala_processing

abstract class SPApplet extends processing.core.PApplet {
  applet =>

  type Dimension = java.awt.Dimension
    
  implicit def pair2Dimension(p: (Int, Int)): Dimension = new Dimension(p._1, p._2)

  private var _swing: Option[Swing] = None
  def swing = _swing
  private[scala_processing] def swing_=(swing: Swing) {
    _swing foreach {
      old =>
      applet.remove(old)
      old.removeAll()
    }
    _swing = Option(swing)
    _swing foreach { applet add }
  }
  
  private var _scene: Scene = new Scene(applet) { override def register() {} }
  def scene = _scene
  private[scala_processing] def scene_=(nextScene: Scene) {
    swing   = null
    _scene  = nextScene
  }  
  
  val screenSize: java.awt.Dimension

  def size(irenderer: String) {
    size(screenSize.width, screenSize.height, irenderer)
  }
  
  def title = if (frame != null) frame.getTitle else ""
  def title_=(title: String) = if (frame != null) frame.setTitle(title)

  var mouseWheelRotation = 0
  
  addMouseWheelListener(new java.awt.event.MouseWheelListener() {
    def mouseWheelMoved(e: java.awt.event.MouseWheelEvent) {
      mouseWheelRotation = e.getWheelRotation()
      scene.mouseWheelMoved()
    }
  })
  
  private var _isKeyPressed   = false
  private var _isMousePressed = false

  def isKeyPressed   = _isKeyPressed
  def isMousePressed = _isMousePressed

  override def keyPressed() {
    _isKeyPressed = true
    scene.keyPressed()
  }
  override def keyReleased() {
    _isKeyPressed = false
    scene.keyReleased()
  }
  override def keyTyped() = scene.keyTyped()

  override def mousePressed() {
    if (!isFocusOwner) requestFocus()    
    _isMousePressed = true
    scene.mousePressed()
  }
  override def mouseReleased() {
    _isMousePressed = false
    scene.mouseReleased()
  }
  override def mouseDragged() {
    scene.mouseDragged()
  }

  override def draw() = scene.draw()

  override def paint(screen: java.awt.Graphics) {
    super.paint(screen)
    swing foreach { _.paint(screen) }
  }
  
  protected def createFrame() {
    import javax.swing.JFrame
    
    val frame = new JFrame
    
    frame.getContentPane.add(applet)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setResizable(false)
    
    applet.frame = frame
    applet.setPreferredSize(screenSize)
    
    frame.pack()
    frame.setLocationRelativeTo(null)
    
    applet.init()
    while (applet.defaultSize && !applet.finished) Thread.sleep(5)
   
    frame.setVisible(true)
  }
  
  def main(args: Array[String]) =
    javax.swing.SwingUtilities.invokeLater(
      new Runnable() { def run() = createFrame() }
    )
}
 
class Scene(applet: SPApplet) extends NotNull {
  register()
  def register(): Unit = applet.scene = this
  def draw() {}
  def keyPressed() {}
  def keyReleased() {}
  def keyTyped() {}
  def mousePressed() {}
  def mouseReleased() {}
  def mouseDragged() {}
  def mouseWheelMoved() {}
}

trait MyUtil {
  this: { val applet: SPApplet } =>

  import processing.core.PGraphics     
  import applet.{ mouseX, mouseY }

  // TK: めんどいから四角形同士の動作を確認してない^o^ たぶんあってる(ｷﾘｯ
  def contains
  (x1: Int, y1: Int, w1: Int, h1: Int)
  (x2: Int, y2: Int, w2: Int, h2: Int) : Boolean = {
    x2 + w2 > x1 && x2 < x1 + w1 && y2 + h2 > y1 && y2 < y1 + h1
  }

  def contains(x: Int, y: Int, w: Int, h: Int, px: Int, py: Int): Boolean =
    contains(x, y, w, h)(px, py, 0, 0)
  
  def mouseContains(x: Int, y: Int, w: Int, h: Int): Boolean =
    contains(x, y, w, h)(mouseX, mouseY, 0, 0)

  def drawPGraphics(g: PGraphics)(draw: PGraphics => Unit) {
    g.beginDraw()
    draw(g)
    g.endDraw()
  }
  
  // TK: こういうの標準でありませんか？あったら教えてぴょ
  def rangeOfNumber[T: Ordering](v: T, min: T, max: T): T = {
    val ord = implicitly[Ordering[T]]

    if (ord.lt(v, min)) min
    else if (ord.gt(v, max)) max
    else v
  }
}

class Swing(applet: SPApplet) extends javax.swing.JPanel(null) {
  setOpaque(false)
  setSize(applet.screenSize)  
  setPreferredSize(applet.screenSize)
  applet.swing = this
}

class SwingManager(val applet: SPApplet) extends NotNull {  
  val swing = new Swing(applet)
  
  trait Component extends javax.swing.JComponent {
    import java.awt.Graphics
    
    swing add this
    
    override def paintComponent(g: Graphics) = super.paintComponent(g)
    override def paint(g: Graphics) = paintComponent(g)
  }
}

class TextManager(applet: SPApplet) extends SwingManager(applet) with MyUtil {
  val textFields = scala.collection.mutable.ArrayBuffer[TextField]()
  def apply(symbol: Symbol): Option[TextField] = textFields.find(_.symbol == symbol)

  def toInt(symbol: Symbol, default: Int = 0): Int =
    try {
      apply(symbol).map(_.getText).getOrElse("").toInt
    } catch {
      case _ => default
    }
    
  def toText(symbol: Symbol, default: String = ""): String =
    apply(symbol).map(_.getText).getOrElse(default)
  
  class TextField(val symbol: Symbol) extends javax.swing.JTextField with Component {
    textField =>

    textFields += textField
    import java.awt.event.{ ActionListener, ActionEvent }
    
    def enter(action: TextField => Unit): TextField = {
      this addActionListener new ActionListener() {
        override def actionPerformed(e: ActionEvent) = action(textField)
      }
      this
    }
  }

  class ValidateField(symbol: Symbol) extends TextField(symbol) {
    def validateValue() {}
    def updateValue() {}
    
    import java.awt.event.{ FocusAdapter, FocusEvent, ActionListener, ActionEvent }
    addFocusListener(new FocusAdapter() {
      override def focusLost(e: FocusEvent): Unit = { validateValue(); updateValue() }
    })
    addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = { validateValue(); updateValue() }
    })
  }

  class IntField(symbol: Symbol, min: Int, max: Int) extends ValidateField(symbol) {
    val defaultValue = min
    
    private var _value = 0
    def value = _value
    def value_=(v: Int) {
      _value = rangeOfNumber(v, min, max)
      super.setText(_value.toString)
    }

    override def setText(text: String) {
      super.setText(text)
      validateValue()
    }

    override def validateValue() {
      value = try {
        getText.toInt
      } catch {
        case _ => defaultValue
      }
    }
    
    import javax.swing.text.{ PlainDocument, AttributeSet }
    setDocument(new PlainDocument() {
      override def insertString(offs: Int, str: String, a: AttributeSet) {
        try {
          str.toInt
          super.insertString(offs, str, a)
        } catch { case _ => return }
      }
    })      
  }      
}

object ButtonStatus {
  case class Value(id: Int) extends NotNull

  val UP       = Value(0)
  val OVER     = Value(1)
  val DOWN     = Value(2)
  val DISABLED = Value(3)  
}

class ButtonManager(val applet: SPApplet) extends NotNull with MyUtil {
  buttonManager =>
    
  import processing.core.{ PImage, PVector, PConstants }
  
  protected val buttons = scala.collection.mutable.ArrayBuffer[Button]()
  private var _isLock = false
  private def isLock_=(isLock: Boolean) { _isLock = isLock }
  def isLock = _isLock

  class Button(initImages: List[PImage]) extends NotNull {
    private var _images: List[PImage] = Nil
    def images = _images
    def images_=(images: List[PImage]) {
      _images = images match {
        case List(_, _, _, _) => _images
        case List(up, over, down) => List(up, over, down, up)
        case List(one) => List(one, one, one, one)
        case _ => throw new java.lang.IllegalArgumentException("images.size should be 1, 3, 4")
      }
    }
    images = initImages

    private var _x = 0
    def x = _x
    def x_=(x: Int)  { _x  = x }
    def addX(x: Int) { _x += x }

    private var _y = 0
    def y = _y
    def y_=(y: Int)  { _y  = y }
    def addY(y: Int) { _y += y }

    var fixedWidth  = 0
    var fixedHeight = 0
    
    def image  = images(status.id)
    def width  = if (fixedWidth  == 0) image.width  else fixedWidth
    def height = if (fixedHeight == 0) image.height else fixedHeight
    
    var action: Option[Button => Unit] = None
    def action(f: Button => Unit): Button = {
      action = Option(f)
      this
    }
    def action(f: => Unit): Button = {
      action = Option ( (b: Button) => { f } )
      this
    }
    
    object status {
      var value  = ButtonStatus.UP
      def id     = value.id
      
      def isUp   = value == ButtonStatus.UP
      def isOver = value == ButtonStatus.OVER
      def isDown = value == ButtonStatus.DOWN
      def isDisabled = value == ButtonStatus.DISABLED

      def up()   { value = ButtonStatus.UP }
      def over() { value = ButtonStatus.OVER }
      def down() { value = ButtonStatus.DOWN }
      def disabled() { value = ButtonStatus.DISABLED }
    }
        
    def checkMouse: Boolean = {
      if (status.isDisabled) return false
      
      val isOver = buttonManager.isOverMouse(this)
      val result = isOver && buttonManager.mouseClicked(this)

      if (mousePressed) {
        if (isOver && !buttonManager.isLock) {
          status.down()
          buttonManager.isLock = true
        }
      } else {
        if (isOver) status.over() else status.up()
        buttonManager.isLock = false
      }
      result
    }

    def draw() = applet.image(image, x, y)
    def draw(width: Int, height: Int) = applet.image(image, x, y, width, height)
  }  

  def isOverMouse(button: Button): Boolean =
    applet.mouseX > button.x &&
    applet.mouseX < button.x + button.width &&
    applet.mouseY > button.y &&
    applet.mouseY < button.y + button.height

  /**
   * override 例
   * applet.mousePressed - マウスのどのボタンでも true
   * applet.mousePressed && applet.mouseButton == PConstants.LEFT
   *   - 左クリックの時 true
   */
  def mousePressed: Boolean =
    applet.isMousePressed && applet.mouseButton == PConstants.LEFT
  
  /**
   * override 例
   * mousePressed - ボタンを押してる間ずっと true
   * mousePressed && button.status.isOver
   *   - 押した時に true
   * !mousePressed && button.status.isDown
   *   - 押して離した時に true
   */
  def mouseClicked(button: Button): Boolean =
    !mousePressed && button.status.isDown

  def createButton(images: List[PImage]) = new Button(images)
  
  def register(images: List[PImage], x: Int = 0, y: Int = 0): Button = {
    val button = createButton(images)
    button.x = x
    button.y = y
    buttons += button
    button
  }

  def unregister(button: Button): Button = {
    buttons -= button
    button
  }

  def clear(): Unit = while(!buttons.isEmpty) { unregister(buttons.head) }

  def checkMouse(button: Button) {
    if (button.checkMouse) button.action.foreach { _(button) }
  }
  
  def checkMouse() {
    buttonManager.buttons foreach { checkMouse }
  }
  
  def draw() = buttons.foreach(_.draw())
}

class ListManager(applet: SPApplet) extends ButtonManager(applet) {
  listManager =>
 
  import processing.core.PImage

  var x = 0
  var y = 0
  var width  = 0
  var height = 0
  
  var scrollWidth = 0
  def isScrollActive = scrollWidth != 0

  def fullWidth = width + scrollWidth
  
  def top    = y
  def left   = x
  def bottom = y + height
  def right  = x + fullWidth

  private var _focus: Option[Button] = None
  def focus = _focus
  def focus_=(option: Option[Button]) { _focus = option }

  private var _scroll = 0
  def scroll = _scroll
  def scroll_=(scroll: Int) {
    _scroll = rangeOfNumber(scroll, 0, overScroll)
  }

  private var upperOverScroll = 0
  private var underOverScroll = 0
  private def overScroll = upperOverScroll + underOverScroll

  var scrollBarButton:  Option[Button] = None
  def scrollBarButton_=(images: List[PImage]) {
    scrollBarButton = Option(
      new Button(images)
    )
  }
  
  var scrollUpButton:   Option[Button] = None
  def scrollUpButton_=(images: List[PImage]) {
    scrollUpButton = Option(
      new Button(images).action { listManager.scroll -= 1 }
    ) 
  }
  
  var scrollDownButton: Option[Button] = None
  def scrollDownButton_=(images: List[PImage]) {
    scrollDownButton = Option(
      new Button(images).action { listManager.scroll += 1 }
    )
  }

  def scrollButtons = List(scrollUpButton, scrollDownButton, scrollBarButton)

  var scrollBackground: Option[PImage] = None
  def scrollBackground_=(image: PImage) { scrollBackground = Option(image) }
  def scrollBackground_=(color: Int)(implicit gg: GraphicsGenerator) {
    val c = gg.rgb(color)
    scrollBackground = gg.createAndDrawPImage(scrollWidth, height) { _.background(c._1, c._2, c._3) }
  }
  
  private var _background: Option[PImage] = None
  def background = _background
  def background_=(image: PImage) {
    _background = Option(image)
    _background foreach {
      bg =>
      width  = bg.width
      height = bg.height
    }    
  }
  def background(width: Int, height: Int, color: Int)(implicit gg: GraphicsGenerator) {
    val c = gg.rgb(color)
    background = gg.createAndDrawPImage(width, height) { _.background(c._1, c._2, c._3) }
  }
  
  def mouseContains: Boolean = mouseContains(x, y, fullWidth, height)

  override def draw() {
    background foreach { applet.image(_, x, y) }
    
    listManager.upperOverScroll = 0    
    listManager.underOverScroll = 0
    var nextY = y

    buttons.zipWithIndex foreach {
      case (button, index) =>
        
      val bottomY = nextY + button.height
      if (index < listManager.scroll) {
        button.status.disabled()
        listManager.upperOverScroll += 1
      } else if (bottomY - y > height) {
        button.status.disabled()        
        listManager.underOverScroll += 1
      } else {
        if ( button.status.isDisabled ) button.status.up()
        button.x = x
        button.y = nextY
        nextY = bottomY

        button.draw()
      }
    }

    if (listManager.isScrollActive) {
      val scrollX = listManager.x + listManager.width
      def drawScrollButton(button: Button, y: Int, height: Int) {
        button.x = scrollX
        button.y = y
        button.draw(listManager.scrollWidth, height)
      }
      
      scrollBackground foreach {
        applet.image(_, scrollX, listManager.y, listManager.scrollWidth, listManager.height)
      }

      var scrollBarTop    = listManager.y
      var scrollBarHeight = listManager.height
      
      scrollUpButton foreach {
        button =>
        scrollBarTop    += button.height
        scrollBarHeight -= button.height
        
        drawScrollButton(button, listManager.y, button.height)
      }
      
      scrollDownButton foreach {
        button =>
        scrollBarHeight -= button.height
        drawScrollButton(button, listManager.bottom - button.height, button.height)  
      }
      
      scrollBarButton foreach {
        button =>
        val size = listManager.buttons.size
        val rate = listManager.overScroll.toFloat / size
        val height = if (size == 0) {
          scrollBarHeight
        } else {
          ( scrollBarHeight - (scrollBarHeight * rate) ).toInt          
        }
        var y = ( (scrollBarHeight.toFloat / size) * listManager.scroll ).toInt
        
        button.fixedHeight = height
        drawScrollButton(button, scrollBarTop + y, height)
      }
    }
  }
  
  override def checkMouse() {
    scrollButtons foreach { _.foreach(checkMouse) }
    super.checkMouse()

    scrollBarButton foreach {
      button =>
      if (button.status.isDown) {
        val y = applet.mouseY - listManager.y
        if (y > 0 && y < listManager.height) {
          val rate = listManager.height.toFloat / listManager.buttons.size
          val centerY = ( y - (button.height >> 1) ).toFloat
          listManager.scroll = ( centerY / rate).toInt
        }
      }
    }
  }
  
  def remove(): Option[Button] = {
    val removeFocus = focus
    focus foreach {
      button =>
      val index = buttons.indexOf(button)
      unregister(button)
      focus = if (buttons.isEmpty) {
        None
      } else {
        val nextIndex = rangeOfNumber(index, 0, buttons.size - 1)
        Option(buttons(nextIndex))
      }
      if (listManager.underOverScroll == 0) listManager.scroll -= 1
    }
    removeFocus
  }
  
  override def clear() {
    super.clear()
    focus = None
  }
}

class GraphicsGenerator(applet: processing.core.PApplet) extends NotNull {
  import processing.core.{ PGraphics, PImage }
  import processing.core.PConstants.{ JAVA2D, ARGB, HSB, CENTER, LEFT, RIGHT }
  
  def rgb(c: Int): (Float, Float, Float) = {
    val g = applet.g
    (g.red(c), g.green(c), g.blue(c))
  }

  def hsb(c: Int): (Float, Float, Float) = {
    val g = applet.g
    (g.hue(c), g.saturation(c), g.brightness(c))
  }

  def createLabel(text: String, width: Int, height: Int, size: Int,
                  frontColor: Int, backColor: Int = -1,
                  frameWeight: Int = 0, frameColor: Int = -1, 
                  align: Int = CENTER): PImage = {
    createAndDrawPImage(width, height) {
      g =>

      g.smooth

      if (backColor >= 0) {
        val c = rgb(backColor)
        g.background(c._1, c._2, c._3)
      }

      if (frameWeight > 0) {
        val c = rgb(frameColor)
        g.noFill()
        g.strokeWeight(frameWeight)
        g.stroke(c._1, c._2, c._3)
        g.rect(1, 1, width - 2, height - 2)
      }
      
      val c = rgb(frontColor)
      g.fill(c._1, c._2, c._3)
      g.textFont(applet.createFont("", size))
      g.textAlign(align)
      val des = g.textDescent.toInt
      val x: Int = align match {
        case LEFT   => 2
        case CENTER => (width >> 1)
      }
      g.text(text, x, (height >> 1) + des + (des >> 1) )
    }
  }
  
  def createCircle(size: Int, hue: Int): PImage = createCircle(size, size, hue)
  def createCircle(width: Int, height: Int, hue: Int): PImage = {        
    val halfW = width >> 1
    val halfH = height >> 1
    val weight = 2

    // TK: 紛らわしいので、いつか +1 を消したい。
    // +1 しないと右と下の stroke が欠けてしまうっぽいから付けてるっぽい。
    createAndDrawPImage(width + 1, height + 1) {
      g =>
        
      def ellipse(w: Int, h: Int) = g.ellipse(halfW, halfH, w - weight, h - weight)
      
      g.colorMode(HSB, 255)
      g.smooth()
      
      g.strokeWeight(weight)
      g.stroke(hue, 255, 100)
      g.fill(hue, 255, 255)
      ellipse(width, height)
      g.noStroke()

      val loop = (if (halfW < halfH) halfW else halfH) - weight
      val div = 255.0f / loop
      (1 to loop) foreach {
        x =>
        g.fill(hue, 255 - (x * div), 255)
        ellipse(width - (x << 1), height - (x << 1))
      }
    }
  }  
  
  def createAndDrawPImage(width: Int, height: Int)(draw: PGraphics => Unit): PImage = {
    val g: PGraphics = applet.createGraphics(width, height, JAVA2D)
    
    g.beginDraw()
    draw(g)
    g.endDraw()

    val image = applet.createImage(width, height, ARGB)
    image.set(0, 0, g)
    g.dispose

    image
  }
}

class LayoutXML(val elem: scala.xml.NodeSeq) {
  def this(filename: String) = this(
    try {
      scala.xml.XML.loadFile(filename)
    } catch {
      case ex =>
      Console.err.println(ex)
      <dummy></dummy>
    }      
  )
  
  import java.awt.Rectangle
  
  val layouts = elem \ "layout"

  def apply(symbol: Symbol)(f: Rectangle => Unit): Unit = f(rect(symbol))
  def apply(name: String)(f: Rectangle => Unit): Unit = f(rect(name))

  def rect(symbol: Symbol): Rectangle = rect(symbol.name)
  def rect(name: String): Rectangle = {
    layouts.find(xml => (xml \ "name").text == name).map {
      xml =>
      try {
        new Rectangle(
          (xml \ "x").text.toInt,
          (xml \ "y").text.toInt,
          (xml \ "width").text.toInt,
          (xml \ "height").text.toInt
        )
      } catch {
        case ex =>
        Console.err.println("layout load error: " + ex)
        new Rectangle(0, 0, 0, 0)
      }
    } getOrElse {
      Console.err.println("not found: " + name)
      new Rectangle(0, 0, 0, 0)
    }
  }
}
