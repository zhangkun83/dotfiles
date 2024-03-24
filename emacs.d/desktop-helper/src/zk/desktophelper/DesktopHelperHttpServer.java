package zk.desktophelper;

import static zk.desktophelper.Protocol.RESPONSE_HEADER_ERROR;
import static zk.desktophelper.Protocol.RESPONSE_HEADER_OK;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.logging.Logger;
import zk.desktophelper.Protocol.Message;

/**
 * An embedded HTTP server that supports redirecting to URLs, view and setting the embedded
 * clipboard.
 */
final class DesktopHelperHttpServer {
  private static final Logger logger =
      Logger.getLogger(DesktopHelperHttpServer.class.getName());

  private final HttpServer httpServer;
  private final int port;
  private final boolean portOpenToNetwork;
  private final Backend backend;

  private final HttpHandler handlerOpenUrl = new HttpHandler() {
      @Override
      public void handle(HttpExchange t) throws IOException {
        String urlCopy = backend.getUrlToOpen();
        if (urlCopy == null) {
          httpRespondHtmlMessage(t, "<p>No URL was sent to " + backend.getName() + ".</p>");
        } else {
          t.getResponseHeaders().set("Location", urlCopy);
          t.sendResponseHeaders(302, 0);
        }
      }
    };
  
  private final HttpHandler handlerViewClip = new HttpHandler() {
      @Override
      public void handle(HttpExchange t) throws IOException {
        String clipboardCopy;
        try {
          clipboardCopy = backend.getClipboard();
        } catch (Exception e) {
            httpRespondHtmlMessage(
                t,
                "<p>Failed to retrieve clipboard.  Exception: " + e);
            return;
        }
        if (clipboardCopy == null) {
          httpRespondHtmlMessage(t, "<p>" + backend.getName() + "'s clipboard is empty.</p>");
        } else {
          if (t.getRequestURI().getPath().endsWith("/viewcliphtml")) {
            t.getResponseHeaders().set("Content-type", "text/html;charset=utf-8");
          } else {
            t.getResponseHeaders().set("Content-type", "text/plain;charset=utf-8");
          }
          byte[] responseBytes = clipboardCopy.getBytes("UTF-8");
          t.sendResponseHeaders(200, responseBytes.length);
          OutputStream os = t.getResponseBody();
          os.write(responseBytes);
          os.close();
        }
      }
    };

  private static void httpRespondHtmlMessage(HttpExchange t, String html) throws IOException {
    t.getResponseHeaders().set("Content-type", "text/html;charset=utf-8");
    byte[] responseBytes = html.getBytes("UTF-8");
    t.sendResponseHeaders(200, responseBytes.length);
    OutputStream os = t.getResponseBody();
    os.write(responseBytes);
    os.close();
  }

  private final HttpHandler handlerSetClip = new HttpHandler() {
      @Override
      public void handle(HttpExchange t) throws IOException {
        String httpMethod = t.getRequestMethod();
        if ("POST".equalsIgnoreCase(httpMethod)) {
          int length = Integer.parseInt(t.getRequestHeaders().getFirst("Content-length"));
          InputStream is = t.getRequestBody();
          String content = new String(is.readAllBytes(), "UTF-8");
          // The body is prepended with the key from the form ("content="), need to remove it.
          content = content.substring("content=".length());
          // Convert HTTP line breaks to unix style
          content = content.replaceAll("\r\n", "\n");
          // The value ends with a newline as part of the http protocol.  Also need to remove it.
          content = content.substring(0, content.length() - 1);
          backend.setClipboard(content);
          logger.info("Set clipboard from HTTP request (" + content.length() + " chars)");
          t.getResponseHeaders().set("Location", "/viewclip");
          t.sendResponseHeaders(302, 0);
        } else {
          httpRespondHtmlMessage(
              t,
              "<p>Set " + backend.getName() + "'s clipboard:</p>\n"
              + "<form action=\"/setclip\" method=\"post\" enctype=\"text/plain\">\n"
              + "<textarea id=\"content\" name=\"content\" rows=\"10\" cols=\"100\"></textarea>\n"
              + "<br><br>\n"
              + "<input type=\"submit\" value=\"Submit\"></form>");
        }
      }
    };

  DesktopHelperHttpServer(int port, boolean portOpenToNetwork, Backend backend) throws IOException {
    this.port = port;
    this.portOpenToNetwork = portOpenToNetwork;
    this.backend = backend;
    httpServer = HttpServer.create(
        portOpenToNetwork ?
        new InetSocketAddress(port) :
        new InetSocketAddress("127.0.0.1", port),
        10);
    httpServer.createContext("/openurl", handlerOpenUrl);
    httpServer.createContext("/viewclip", handlerViewClip);
    httpServer.createContext("/viewcliphtml", handlerViewClip);
    httpServer.createContext("/setclip", handlerSetClip);
    httpServer.setExecutor(null);
  }

  void start() {
    httpServer.start();
    logger.info("HTTP server started on port " + port
        + (portOpenToNetwork ? " for ALL network interfaces"
            : " for 127.0.0.1 only"));
    if (portOpenToNetwork) {
      logger.warning(
          "HTTP port is open to network.  Use this only in VMs whose ports can"
          + " only be accessed from the local machine.");
    }
  }

  interface Backend {
    // Nullable
    String getClipboard() throws Exception;
    void setClipboard(String content);
    // Nullable
    String getUrlToOpen();
    String getName();
  }
}
