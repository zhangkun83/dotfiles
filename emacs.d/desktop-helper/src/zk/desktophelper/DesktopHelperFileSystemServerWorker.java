package zk.desktophelper;

import static zk.desktophelper.Protocol.RESPONSE_HEADER_ERROR;
import static zk.desktophelper.Protocol.RESPONSE_HEADER_OK;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Logger;
import zk.desktophelper.Protocol;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperFileSystemServerWorker extends MessageWorker {
  private static final Logger logger =
      Logger.getLogger(DesktopHelperFileSystemServerWorker.class.getName());

  private static final Path BASE_DIR =
      Paths.get(System.getProperty("user.home"), ".tmp-desktophelper");
  private static final Path PATH_CLIPBOARD = BASE_DIR.resolve("clip.txt");
  private static final Path PATH_CLIPBOARD_HTML = BASE_DIR.resolve("clip.html");
  private static final Path PATH_URL_OPENER = BASE_DIR.resolve("openurl.html");

  private static void writeFile(Path path, String content) throws IOException {
    Files.write(path, content.getBytes("UTF-8"));
  }

  @Override
  public void workOnConnection(MessageStream stream) throws IOException {
    while (true) {
      Message msg = stream.readMessage();
      logger.info("Received: " + msg);
      if (msg.header.equals("store-to-clipboard")) {
        try {
          writeFile(PATH_CLIPBOARD, msg.data);
          writeFile(PATH_CLIPBOARD_HTML, msg.data);
          stream.writeMessage(
              RESPONSE_HEADER_OK,
              "Wrote " + msg.data.length() + " chars to "
              + PATH_CLIPBOARD.getFileName() + " and " + PATH_CLIPBOARD_HTML.getFileName());
        } catch (IOException e) {
          stream.writeMessage(RESPONSE_HEADER_ERROR, e.toString());
        }
      } else if (msg.header.equals("retrieve-from-clipboard")) {
        try {
          byte[] bytes = Files.readAllBytes(PATH_CLIPBOARD);
          stream.writeMessage(RESPONSE_HEADER_OK, new String(bytes, "UTF-8"));
        } catch (IOException e) {
          stream.writeMessage(RESPONSE_HEADER_ERROR, e.toString());
        }
      } else if (msg.header.equals("open-url")) {
        String url = msg.data;
        try {
          writeFile(
              PATH_URL_OPENER,
              "<body><script type=\"text/javascript\">\n"
              + "window.location.href = \"" + url + "\";\n"
              + "</script></body>");
          stream.writeMessage(RESPONSE_HEADER_OK, "URL sent to " + PATH_URL_OPENER.getFileName());
        } catch (IOException e) {
          stream.writeMessage(RESPONSE_HEADER_ERROR, e.toString());
        }
      } else if (msg.header.equals("ping")) {
        stream.writeMessage(RESPONSE_HEADER_OK, "Pong!");
      } else {
        stream.writeMessage(RESPONSE_HEADER_ERROR, "Unsupported command: " + msg.header);
      }
    }
  }
}  
