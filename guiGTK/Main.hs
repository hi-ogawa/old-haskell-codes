module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main = do
    initGUI
    Just xml <- xmlNew "hellogtk2hs.glade"
    window   <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit
    label    <- xmlGetWidget xml castToLabel "label1"
    entry    <- xmlGetWidget xml castToEntry "entry1"
    applyBut <- xmlGetWidget xml castToButton "button1"
    closeBut <- xmlGetWidget xml castToButton "button2"
    onClicked closeBut $ widgetDestroy window
    onClicked applyBut $ 
      do name <- get entry entryText
         set label [ labelText := "Hello, " ++ name]
    widgetShowAll window
    mainGUI

{-
<apt-get>
libghc-glib-dev libghc-gtk-dev libghc-glade-dev
intltool (glade3で必要)
glade (glade3で使うライブラリ揃えるため)


wget http://ftp.gnome.org/pub/GNOME/sources/glade/3.8/glade3-3.8.0.tar.gz
tar xvf glade3-3.8.0.tar.gz
cd glade3-3.8.0
.configure
make
sudo make install
glade-3

-- <cabal>
-- glade
-- gtk
-}
