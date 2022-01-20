param($Title)
$ErrorActionPreference = "Stop"
[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] > $null
$Template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02)

$RawXml = [xml] $Template.GetXml()
($RawXml.toast.visual.binding.text | Where-Object { $_.id -eq "1" }).AppendChild($RawXml.CreateTextNode($Title)) > $null
($RawXml.toast.visual.binding.text | Where-Object { $_.id -eq "2" }).AppendChild($RawXml.CreateTextNode($args[0])) > $null

$SerializedXml = New-Object Windows.Data.Xml.Dom.XmlDocument
$SerializedXml.LoadXml($RawXml.OuterXml)

$Toast = [Windows.UI.Notifications.ToastNotification]::new($SerializedXml)
# $Toast.Tag = "PowerShell"
# $Toast.Group = "PowerShell"
$Toast.ExpirationTime = [DateTimeOffset]::Now.AddMinutes(30)

$Notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier("Emacs")
$Notifier.Show($Toast);

# this will get my attention!
[console]::beep(1568, 100)
[console]::beep(1480, 100)
[console]::beep(1245, 100)
[console]::beep(880,  100)
[console]::beep(831,  100)
[console]::beep(1244, 100)
[console]::beep(1661, 100)
[console]::beep(2093, 100)
