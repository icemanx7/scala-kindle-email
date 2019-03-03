import java.io.File
import java.util.Properties

import javax.mail._
import javax.mail.internet.{InternetAddress, MimeBodyPart, MimeMessage, MimeMultipart}
import org.json4s._
import org.json4s.native.JsonMethods._
import javax.activation.DataHandler
import javax.activation.DataSource
import javax.activation.FileDataSource
import javax.mail.internet.MimeBodyPart



case class EmailSettings(
                          email: String,
                          password: String,
                          showName: String,
                          pdfPaths: String,
                          kindleEmail: String,
                          fileTypes: List[String]
                        )
sealed trait FileTypes
case object PDF extends FileTypes
case object MOBI extends FileTypes
case object EPUB extends FileTypes

object Main {

  val fileData = "emailConf.json"
  implicit val formats = DefaultFormats

  def getEmailSettings(path: String): EmailSettings = {
    val data = scala.io.Source.fromFile(fileData).mkString
    parse(data).extract[EmailSettings]
  }


  def getExtention(path: String): String  = {
    val period = path.lastIndexOf('.')
    if(period != -1)
      path.substring(period + 1, path.length)
    else
      ""

  }

  def getListOfFiles(settings: EmailSettings): List[File] = {
    val d = new File( settings.pdfPaths)
    if (d.exists && d.isDirectory) {
      println(d.listFiles.toList)
      println(d.listFiles.toList.map(file => getExtention(file.getName)))
      d.listFiles.toList.filter((file) => settings.fileTypes.contains(getExtention(file.getName)))
    } else {
      List[File]()
    }
  }


  def initMailSend(emailSettings: EmailSettings): Unit = {
    val host = "smtp.gmail.com"
    val props = new Properties()
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")
    props.put("mail.smtp.host", host)
    props.put("mail.smtp.port", "587")

    val session = Session.getInstance(props, new Authenticator() {
      override protected def getPasswordAuthentication = new PasswordAuthentication(emailSettings.email, emailSettings.password)
    })

    try {
      val message = new MimeMessage(session)

      message.setFrom(new InternetAddress(emailSettings.email))
      message.setRecipient(Message.RecipientType.TO, new InternetAddress((emailSettings.kindleEmail)))
      message.setSubject("Testing Subject")

      val messageBodyPart = new MimeBodyPart

      messageBodyPart.setText("This is message body")

      val multipart = new MimeMultipart

      multipart.addBodyPart(messageBodyPart)

      val newMessageBodyPart = new MimeBodyPart
      getListOfFiles(emailSettings).foreach(file => {
        val source = new FileDataSource(file.getAbsolutePath)
        newMessageBodyPart.setDataHandler(new DataHandler(source))
        newMessageBodyPart.setFileName(file.getName)
        multipart.addBodyPart(newMessageBodyPart)
      })
      println('RICRD)

      // Send the complete message parts// Send the complete message parts

      message.setContent(multipart)

      // Send message
      Transport.send(message)

      System.out.println("Sent message successfully....")

    }
    catch {
      case e: Exception =>
        throw new RuntimeException(e)
    }

  }






  def main(args: Array[String]): Unit = {
    println(initMailSend(getEmailSettings(fileData)))

  }

}