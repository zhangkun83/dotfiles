package zk.desktophelper;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

class NotificationWindow extends JFrame {
  private final JTextArea content;

  NotificationWindow(String title, String message) {
    setTitle(title);
    Image icon = Toolkit.getDefaultToolkit().createImage(getClass().getResource("trayicon.png"));
    setResizable(false);
    setIconImage(icon);
    setLayout(new FlowLayout(FlowLayout.CENTER));
    content = new JTextArea(message, 3, 30);
    content.setFont(new Font(Font.SERIF, Font.PLAIN, 18));
    content.setEditable(false);
    content.setLineWrap(true);
    content.setWrapStyleWord(true);
    content.setMargin(new Insets(5, 5, 5, 5));
    getContentPane().setBackground(Color.YELLOW);

    JScrollPane jsp = new JScrollPane(content);
    jsp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    add(jsp);
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setAlwaysOnTop(true);
    pack();
    setLocationRelativeTo(null);
  }
}
