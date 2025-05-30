package zk.desktophelper;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.Timer;

class NotificationWindow extends JFrame {
  private final JTextArea content;
  private final JLabel countDown;
  private final CountDownTimerHandler countDownTimerHandler;
  private final Timer countDownTimer;
  private static final String fontFamily = "Aporetic Sans Mono";
  private static final DateTimeFormatter dateTimeFormatter =
      DateTimeFormatter.ofPattern("HH:mm");

  NotificationWindow(String title, String message) {
    setTitle(title);
    Image icon = Toolkit.getDefaultToolkit().createImage(getClass().getResource("trayicon.png"));
    setResizable(true);
    setIconImage(icon);
    setLayout(new BorderLayout(5, 5));
    getContentPane().setBackground(Color.YELLOW);

    content = new JTextArea(message, 3, 30);
    content.setFont(new Font(fontFamily, Font.PLAIN, 16));
    content.setEditable(false);
    content.setLineWrap(true);
    content.setWrapStyleWord(true);
    content.setMargin(new Insets(5, 5, 5, 5));
    JScrollPane jsp = new JScrollPane(content);
    jsp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    add(jsp, BorderLayout.CENTER);

    countDown = new JLabel(" ", JLabel.CENTER);
    countDown.setFont(new Font(fontFamily, Font.PLAIN, 12));
    add(countDown, BorderLayout.PAGE_END);

    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    setAlwaysOnTop(true);
    pack();
    setLocationRelativeTo(null);
    setVisible(true);

    countDownTimerHandler = new CountDownTimerHandler();
    countDownTimerHandler.update();
    countDownTimer = new Timer(1000, countDownTimerHandler);
    countDownTimer.start();
  }

  private class CountDownTimerHandler implements ActionListener {
    int secondsLeft = 1;
    final LocalDateTime time = LocalDateTime.now();

    @Override
    public void actionPerformed(ActionEvent evt) {
      secondsLeft --;
      update();
    }

    void update() {
      if (secondsLeft > 0) {
        countDown.setText("You may close it in " + secondsLeft + "s");
      } else {
        countDown.setText(dateTimeFormatter.format(time));
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        countDownTimer.stop();
      }
      pack();
    }
  }
}
