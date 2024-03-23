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
 * A fallback worker to use by the proxy when it cannot connect to the real server.  This worker
 * starts an HTTP server that supports redirecting to URLs, view and setting the embedded clipboard.
 */
final class DesktopHelperFallbackWorker {
  private static final Logger logger =
      Logger.getLogger(DesktopHelperFallbackWorker.class.getName());

  private final HttpServer httpServer;
  private final HttpHandler handlerOpenUrl = new HttpHandler() {
      @Override
      public void handle(HttpExchange t) throws IOException {
        String urlCopy = url;
        if (urlCopy == null) {
          httpRespondHtmlMessage(t, "<p>No URL was sent to the Desktop Helper.</p>");
        } else {
          t.getResponseHeaders().set("Location", urlCopy);
          t.sendResponseHeaders(302, 0);
        }
      }
    };
  
  private final HttpHandler handlerViewClip = new HttpHandler() {
      @Override
      public void handle(HttpExchange t) throws IOException {
        String clipboardCopy = clipboard;
        if (clipboardCopy == null) {
          httpRespondHtmlMessage(t, "<p>Desktop Helper's clipboard is empty.</p>");
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
          logger.info("Set clipboard from HTTP request (" + content.length() + " chars)");
          clipboard = content;
          t.getResponseHeaders().set("Location", "/viewclip");
          t.sendResponseHeaders(302, 0);
        } else {
          httpRespondHtmlMessage(
              t,
              "<p>Set the DesktopHelper clipboard:</p>\n"
              + "<form action=\"/setclip\" method=\"post\" enctype=\"text/plain\">\n"
              + "<textarea id=\"content\" name=\"content\" rows=\"10\" cols=\"100\"></textarea>\n"
              + "<br><br>\n"
              + "<input type=\"submit\" value=\"Submit\"></form>");
        }
      }
    };

  private volatile String clipboard;
  private volatile String url;

  DesktopHelperFallbackWorker(int httpPort, boolean httpOpenToNetwork) throws IOException {
    httpServer = HttpServer.create(
        httpOpenToNetwork ?
        new InetSocketAddress(httpPort) :
        new InetSocketAddress("127.0.0.1", httpPort),
        10);
    httpServer.createContext("/openurl", handlerOpenUrl);
    httpServer.createContext("/viewclip", handlerViewClip);
    httpServer.createContext("/viewcliphtml", handlerViewClip);
    httpServer.createContext("/setclip", handlerSetClip);
    httpServer.setExecutor(null);
  }

  void startHttpServer() {
    httpServer.start();
  }

  Message handleRequest(Message request) {
    logger.info("Handling " + request.header);
    if (request.header.equals("store-to-clipboard")) {
      clipboard = request.data;
      return new Message(
          RESPONSE_HEADER_OK, "Stored " + request.data.length() + " chars in fallback worker");
    }
    if (request.header.equals("retrieve-from-clipboard")) {
      String clipboardCopy = clipboard;
      if (clipboardCopy == null) {
        return new Message(RESPONSE_HEADER_ERROR, "Fallback worker doesn't have data in clipboard");
      }
      return new Message(RESPONSE_HEADER_OK, clipboardCopy);
    }
    if (request.header.equals("open-url")) {
      url = request.data;
      return new Message(RESPONSE_HEADER_OK, "URL stored in fallback worker");
    }
    return new Message(
        RESPONSE_HEADER_ERROR, "Fallback worker doesn't support " + request.header);
  }
}
