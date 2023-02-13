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
      } else {
        writeMessage(out, new Message("ERROR", "Unsupported command: " + msg.header));
      }
    }
  }
}  
