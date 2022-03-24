package blog.java;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.Set;


public class MioServer extends Server {
  
  // private Selector selector = Selector.open();
  // private ServerSocketChannel crunchifySocket = ServerSocketChannel.open();

  public MioServer() {}

  private void println(Object x) {
    System.out.println(x);
  }

  @Override
  public void bind(int port, String host) {
    println("unimplemented!");
    return;
  }

  @Override
  public void serve() {
    println("unimplemented!");
    return;
  }
}
