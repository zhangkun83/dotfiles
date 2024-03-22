package zk.desktophelper;

import static zk.desktophelper.Protocol.RESPONSE_HEADER_ERROR;
import static zk.desktophelper.Protocol.RESPONSE_HEADER_OK;

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

  DesktopHelperServerWorker(boolean useSystemNotifications) {
    TrayIcon trayIcon = null;
    if (useSystemNotifications) {
      try {
        SystemTray tray = SystemTray.getSystemTray();
        Image image =
            Toolkit.getDefaultToolkit().createImage(getClass().getResource("trayicon.png"));
        trayIcon = new TrayIcon(image, "Desktop Helper Server");
        trayIcon.setImageAutoSize(true);
        trayIcon.setToolTip("Desktop Helper Notifications");
        tray.add(trayIcon);
      } catch (Exception e) {
        logger.warning("Failed to get SystemTray. Reason=" + e);
      }
    }
    this.trayIcon = trayIcon;
    new Thread(() -> {
          // Allow the icon to be added to OS'es tray.  Otherwise the message may be lost or the
          // icon may not display properly in the message.
          try {
            Thread.sleep(2000);
          } catch (Exception e) {}
          displayNotification("Notifications sent to the Desktop Helper will show like this.");
    }).start();
  }

  private boolean displayNotification(String msg) {
    if (trayIcon != null) {
      trayIcon.displayMessage(msg, "Desktop Helper", TrayIcon.MessageType.INFO);
      return true;
    } else {
      new NotificationWindow("Desktop Helper", msg);
      return true;
    }
  }

  @Override
  public void workOnConnection(MessageStream stream) throws IOException {
    while (true) {
      Message msg = stream.readMessage();
      logger.info("Received: " + msg);
      if (msg.header.equals("store-to-clipboard")) {
        clipboard.setContents(new StringSelection(msg.data), null);
        stream.writeMessage(
            RESPONSE_HEADER_OK, "Stored " + msg.data.length() + " chars to clipboard");
      } else if (msg.header.equals("retrieve-from-clipboard")) {
        try {
          String content = (String) clipboard.getData(DataFlavor.stringFlavor);
          stream.writeMessage(RESPONSE_HEADER_OK, content);
        } catch (UnsupportedFlavorException e) {
          stream.writeMessage(RESPONSE_HEADER_ERROR, e.toString());
        }
      } else if (msg.header.equals("open-url")) {
        String os = System.getProperty("os.name");
        String program = null;
        if (os.toLowerCase().contains("win")) {
          program = "c:/Program Files/Google/Chrome/Application/chrome.exe";
        } else if (os.toLowerCase().contains("linux")) {
          program = "google-chrome";
        }
        if (program == null) {
          stream.writeMessage(RESPONSE_HEADER_ERROR, "Unsupported OS for open-url: " + os);
        } else {
          try {
            Runtime.getRuntime().exec(new String[]{program, msg.data});
            stream.writeMessage(RESPONSE_HEADER_OK, "URL sent to " + program);
          } catch (IOException e) {
            stream.writeMessage(RESPONSE_HEADER_ERROR, e.toString());
          }
        }
      } else if (msg.header.equals("notify")) {
        boolean ret = displayNotification(msg.data);
        if (ret) {
          stream.writeMessage(RESPONSE_HEADER_OK, "displayNotification() succeeded");
        } else {
          stream.writeMessage(RESPONSE_HEADER_ERROR, "displayNotification() failed");
        }
      } else if (msg.header.equals("ping")) {
        stream.writeMessage(RESPONSE_HEADER_OK, "Pong!");
      } else {
        stream.writeMessage(RESPONSE_HEADER_ERROR, "Unsupported command: " + msg.header);
      }
    }
  }
}  
