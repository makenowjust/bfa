package bfa

sealed trait Reader {
  def current: Option[Char]

  def eof: Boolean = current == None

  def next: Reader

  def forward: Reader

  def backward: Reader
}

object Reader {
  def apply(s: String): Reader = ReaderImpl(s, 0, true)

  private final case class ReaderImpl(text: String, offset: Int, forwarding: Boolean) extends Reader {
    def current: Option[Char] =
      if (0 <= index && index < text.length) Some(text(index)) else None

    def next: Reader = ReaderImpl(text, (offset + direction).min(text.length).max(0), forwarding)

    private[this] def index = if (forwarding) offset else offset - 1

    private[this] def direction = if (forwarding) 1 else -1

    def forward: Reader = ReaderImpl(text, offset, true)

    def backward: Reader = ReaderImpl(text, offset, false)
  }
}
