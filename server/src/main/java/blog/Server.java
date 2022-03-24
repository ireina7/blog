package blog.java;


public abstract class Server {
  
  public abstract void bind(int port, String host);
  public abstract void serve();
}