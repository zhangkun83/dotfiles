package zk.desktophelper;

import java.awt.Image;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.logging.Logger;
import zk.desktophelper.Protocol;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperServerWorker extends MessageWorker {
  private static final Logger logger = Logger.getLogger(DesktopHelperServerWorker.class.getName());
  private final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
  private final TrayIcon trayIcon;

  DesktopHelperServerWorker() {
    TrayIcon trayIcon = null;
    try {
      SystemTray tray = SystemTray.getSystemTray();
      Image image = Toolkit.getDefaultToolkit().createImage(getClass().getResource("trayicon.png"));
      trayIcon = new TrayIcon(image, "DesktopHelperServer");
      trayIcon.setImageAutoSize(true);
      trayIcon.setToolTip("DesktopHelper Notifications");
      tray.add(trayIcon);
    } catch (Exception e) {
      logger.warning(
          "Failed to get SystemTray. Notifications will not be displayed. Reason=" + e);
    }
    this.trayIcon = trayIcon;
    if (trayIcon != null) {
      new Thread(() -> {
            // Allow the icon to be added to OS'es tray.  Otherwise the message may be lost or the
            // icon may not display properly in the message.
            try {
              Thread.sleep(2000);
            } catch (Exception e) {}
            displayNotification("DesktopHelper notifications will display here.");
      }).start();
    }
  }

  private boolean displayNotification(String msg) {
    if (trayIcon != null) {
      trayIcon.displayMessage(msg, "DesktopHelperServer", TrayIcon.MessageType.INFO);
      return true;
    }
    return false;
  }

  @Override
  public void workOnConnection(MessageStream stream) throws IOException {
    while (true) {
      Message msg = stream.readMessage();
      logger.info("Received: " + msg);
      if (msg.header.equals("store-to-clipboard")) {
        clipboard.setContents(new StringSelection(msg.data), null);
        stream.writeMessage("OK", "Stored " + msg.data.length() + " chars to clipboard");
      } else if (msg.header.equals("retrieve-from-clipboard")) {
        try {
          String content = (String) clipboard.getData(DataFlavor.stringFlavor);
          stream.writeMessage("OK", content);
        } catch (UnsupportedFlavorException e) {
          stream.writeMessage("ERROR", e.toString());
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
          stream.writeMessage("ERROR", "Unsupported OS for open-url: " + os);
        } else {
          try {
            Runtime.getRuntime().exec(new String[]{program, msg.data});
            stream.writeMessage("OK", "URL sent to " + program);
          } catch (IOException e) {
            stream.writeMessage("ERROR", e.toString());
          }
        }
      } else if (msg.header.equals("notify")) {
        boolean ret = displayNotification(msg.data);
        if (ret) {
          stream.writeMessage("OK", "displayNotification() succeeded");
        } else {
          stream.writeMessage("ERROR", "displayNotification() failed");
        }
      } else if (msg.header.equals("ping")) {
        stream.writeMessage("OK", "Pong!");
      } else {
        stream.writeMessage("ERROR", "Unsupported command: " + msg.header);
      }
    }
  }
}  
