package zk.desktophelper;

import static zk.desktophelper.Protocol.readMessage;
import static zk.desktophelper.Protocol.writeMessage;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Logger;
import zk.desktophelper.Protocol;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperServerWorker implements BlockingServer.Worker {
  private static final Logger logger = Logger.getLogger(DesktopHelperServerWorker.class.getName());
  final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

  @Override
  public void workOnConnection(InputStream in, OutputStream out) throws IOException {
    while (true) {
      Message msg = readMessage(in);
      logger.info("Received: " + msg);
      if (msg.header.equals("store-to-clipboard")) {
        clipboard.setContents(new StringSelection(msg.data), null);
        writeMessage(
            out, new Message("OK", "Stored " + msg.data.length() + " chars to clipboard"));
      } else if (msg.header.equals("retrieve-from-clipboard")) {
        try {
          String content = (String) clipboard.getData(DataFlavor.stringFlavor);
          writeMessage(out, new Message("OK", content));
        } catch (UnsupportedFlavorException e) {
          writeMessage(out, new Message("ERROR", e.toString()));
        }
      } else if (msg.header.equals("open-url")) {
        String os = System.getProperty("os.name");
        String program = null;
        if (os.toLowerCase().contains("win")) {
          program = "c:/Program Files/Google/Chrome/Application/chrome.exe";
        } else if (os.toLowerCase().contains("linux")) {
          program = "/usr/bin/google-chrome";
        }
        if (program == null) {
          writeMessage(out, new Message("ERROR", "Unsupported OS for open-url: " + os));
        } else {
          try {
            Runtime.getRuntime().exec(new String[]{program, msg.data});
            writeMessage(out, new Message("OK", "URL sent to " + program));
          } catch (IOException e) {
            writeMessage(out, new Message("ERROR", e.toString()));
          }
        }
      } else if (msg.header.equals("ping")) {
        writeMessage(out, new Message("OK", "Pong!"));
      } else {
        writeMessage(out, new Message("ERROR", "Unsupported command: " + msg.header));
      }
    }
  }
}  
