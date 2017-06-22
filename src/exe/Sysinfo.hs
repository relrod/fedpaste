{-# LANGUAGE OverloadedStrings #-}
module Sysinfo where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Shelly as S

data Info =
  Info { name :: T.Text
       , commands :: [T.Text]
       } deriving (Eq, Show)

data InfoResult =
    InfoResult { resCommand :: T.Text
               , resOut :: T.Text
               }
  | InfoError
  deriving (Eq, Show)

runCmd :: T.Text -> S.Sh (T.Text, Int)
runCmd cmd = do
  res <- S.run "sh" ["-c", cmd]
  rc <- S.lastExitCode
  return (res, rc)

runInfo :: Info -> IO InfoResult
runInfo info@(Info name' (h:tl)) = do
  (res, rc) <- S.shelly .
               S.errExit False .
               S.print_stdout False .
               S.print_stderr False $
               runCmd h
  if rc == 0
    then return $ InfoResult h res
    else runInfo (info { commands = tl })
runInfo _ = return InfoError

prettyInfo :: T.Text -> InfoResult -> T.Text
prettyInfo name InfoError =
  "* Unable to determine " <> name <> "\n\n"
prettyInfo name (InfoResult cmd out) =
  "* " <> name <> " (" <> cmd <> "):\n\n" <>
  T.unlines (fmap (\t -> "     " <> t) (T.lines out)) <> "\n\n"

osRelease :: Info
osRelease = Info "OS Release"
  [ "lsb_release -ds"
  , "cat /etc/*-release | uniq"
  , "cat /etc/issue"
  , "cat /etc/motd"
  ]

kernel :: Info
kernel = Info "Kernel" ["uname -r; cat /proc/cmdline"]

desktopsRunning :: Info
desktopsRunning = Info "Desktop(s) Running"
  [ "ps -eo comm= | grep -E \
    \'(gnome-session|startkde|startactive|xfce.?-session|fluxbox|blackbox|\
    \hackedbox|ratpoison|enlightenment|icewm-session|od-session|wmaker|wmx|\
    \openbox-lxde|openbox-gnome-session|openbox-kde-session|mwm|e16|fvwm|\
    \xmonad|sugar-session|mate-session|lxqt-session|cinnamon)'"
  ]

desktopsInstalled :: Info
desktopsInstalled = Info "Desktop(s) Installed"
  ["ls -m /usr/share/xsessions/ | sed 's/\\.desktop//g'"]

selinuxStatus :: Info
selinuxStatus = Info  "SELinux Status"
  [ "sestatus"
  , "/usr/sbin/sestatus"
  , "getenforce"
  , "grep -v '^#' /etc/sysconfig/selinux"
  ]

selinuxErrorCount :: Info
selinuxErrorCount = Info "SELinux Error Count"
  ["selinuxenabled && journalctl --since yesterday |grep avc: |grep -Eo"]

cpuModel :: Info
cpuModel = Info "CPU Model"
  [ "grep 'model name' /proc/cpuinfo | awk -F: '{print $2}' | \
    \uniq -c | sed -re 's/^ +//'"
  , "grep 'model name' /proc/cpuinfo"
  ]

sixtyFourBit :: Info
sixtyFourBit = Info "64-bit Support"
  ["grep -q ' lm ' /proc/cpuinfo && echo Yes || echo No"]

hwVirt :: Info
hwVirt = Info "Hardware Virtualization Support"
  ["grep -Eq '(vmx|svm)' /proc/cpuinfo && echo Yes || echo No"]

loadAvg :: Info
loadAvg = Info "Load average" ["uptime"]

memory :: Info
memory = Info "Memory usage" ["free -m", "free"]

topCPUHogs :: Info
topCPUHogs = Info "Top 5 CPU hogs" ["ps axuScnh | sort -rnk3 | head -5"]

topMemHogs :: Info
topMemHogs = Info "Top 5 Memory hogs" ["ps axuScnh | sort -rnk4 | head -5"]

disk :: Info
disk = Info "Disk space usage" ["df -hT''', 'df -h', 'df'"]

block :: Info
block = Info "Block devices" ["blkid", "/sbin/blkid"]

pci :: Info
pci = Info "PCI devices" ["lspci", "/sbin/lspci"]

usb :: Info
usb = Info "USB devices" ["lsusb", "/sbin/lsusb"]

drm :: Info
drm = Info "DRM Information"
  ["journalctl -k -b | grep -o 'kernel:.*drm.*$' | cut -d ' ' -f 2-"]

xorg :: Info
xorg = Info "Xorg modules"
  ["grep LoadModule /var/log/Xorg.0.log ~/.local/share/xorg/Xorg.0.log | \
   \cut -d \" -f 2 | xargs"]

gl :: Info
gl = Info "GL Support"
  ["glxinfo | grep -E 'OpenGL version|OpenGL renderer'"]

xorgErrors :: Info
xorgErrors = Info "Xorg errors"
  ["grep '^\\[.*(EE)' /var/log/Xorg.0.log ~/.local/share/xorg/Xorg.0.log | \
   \cut -d ':' -f 2-"]

kernelBufTail :: Info
kernelBufTail = Info "Kernel buffer tail" ["dmesg | tail"]

reboots :: Info
reboots = Info "Last few reboots" ["last -x -n10 reboot runlevel"]

dnfRepos :: Info
dnfRepos = Info "DNF Repositories"
  [ "dnf -C repolist"
  , "ls -l /etc/yum.repos.d"
  , "grep -v '^#' /etc/yum.conf"
  ]

dnfExtras :: Info
dnfExtras = Info "DNF Extras" ["dnf -C list extras"]

last20Pkgs :: Info
last20Pkgs = Info "Last 20 packages installed"
  ["rpm -qa --nodigest --nosignature --last | head -20"]

infoList :: [Info]
infoList =
  [ osRelease
  , kernel
  , desktopsRunning
  , desktopsInstalled
  , selinuxStatus
  , selinuxErrorCount
  , cpuModel
  , sixtyFourBit
  , hwVirt
  , loadAvg
  , memory
  , topCPUHogs
  , topMemHogs
  , disk
  , block
  , pci
  , usb
  , drm
  , xorg
  , gl
  , xorgErrors
  , kernelBufTail
  , reboots
  , dnfRepos
  , dnfExtras
  , last20Pkgs
  ]

--runInfoList :: IO T.Text
runInfoList =
  mapM_ (\info -> runInfo info >>= putStrLn . T.unpack . prettyInfo (name info))
        infoList
