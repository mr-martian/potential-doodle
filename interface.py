from tkinter import *
import sys
from datatypes import *
from compilelang import *
from gentext import *

class WindowMode(Frame):
    def __init__(self, master, returnTo):
        super().__init__(master)
        self.pack()
        if returnTo:
            self.back = Button(self, text="< Back", command=self.goBack)
            self.back.pack()
    def goBack(self):
        pass

class MainMenu(Frame):
    def __init__(self, master):
        super().__init__(master, relief=SUNKEN)
        self.pack()
        self.label = Label(self, text="Potential Doodle")
        self.label.pack(side="top")
        self.quit = Button(self, text="Exit", command=sys.exit)
        self.quit.pack(side="bottom")
def showmeta(sl,tl):
    print([sl,tl])
def translation(sl,tl):
    print([sl,tl])
class ModeMenu(Frame):
    modes = [
      ['metadata', showmeta],
      ['translate', translation]
    ]
    def __init__(self, master):
        super().__init__(master, relief=SUNKEN)
        master.add(self)
        mode_holder = LabelFrame(self, text="Mode")
        mode_holder.pack(fill="x")
        self.mode_var = StringVar()
        self.mode_var.set(ModeMenu.modes[0][0])
        mode = OptionMenu(mode_holder, self.mode_var, *[x[0] for x in ModeMenu.modes])
        mode.pack(side="top")
        sl_holder = LabelFrame(self, text="Source Language")
        sl_holder.pack(fill="x")
        self.sl_var = IntVar()
        self.sl_label = StringVar()
        self.sl_label.set('[select language]')
        self.sl_menu = Menu(None)
        self.sl = Menubutton(sl_holder, textvariable=self.sl_label, menu=self.sl_menu, relief=RAISED)
        self.sl.pack()
        tl_holder = LabelFrame(self, text="Target Language")
        tl_holder.pack(fill="x")
        self.tl_var = IntVar()
        self.tl_label = StringVar()
        self.tl_label.set('[select language]')
        self.tl_menu = Menu(None)
        self.tl = Menubutton(tl_holder, textvariable=self.tl_label, menu=self.tl_menu, relief=RAISED)
        self.tl.pack()
        self.newlang = StringVar()
        Entry(self, textvariable=self.newlang).pack()
        Button(self, text='Add Language', command=self.loadlang).pack()
        self.list_langs()
    def update_mode(self, issl, lang, name):
        if issl:
            self.sl_var.set(lang)
            self.sl_label.set(name)
        else:
            self.tl_var.set(lang)
            self.tl_label.set(name)
        dict(ModeMenu.modes)[self.mode_var.get()].__call__(self.sl_var.get(), self.tl_var.get())
    def list_langs(self):
        self.sl_menu.destroy()
        self.tl_menu.destroy()
        self.sl_menu = Menu(self.sl)
        self.tl_menu = Menu(self.tl)
        self.sl['menu'] = self.sl_menu
        self.tl['menu'] = self.tl_menu
        for l,n in Language.allnames():
            print([l,n])
            self.sl_menu.add_command(label=n, command=lambda x=l,y=n: self.update_mode(True, x, y))
            self.tl_menu.add_command(label=n, command=lambda x=l,y=n: self.update_mode(False, x, y))
    def loadlang(self):
        try:
            x = int(self.newlang.get())
            loadlang(x)
            for l,n in Language.allnames():
                loadtrans(x,l)
                loadtrans(l,x)
            self.list_langs()
        except:
            print('Unable to load langauge "%s". Sorry.' % self.newlang.get())

def Exit():
    sys.exit()
root = Tk()
root.title('Potential Doodle')
menu = Menu(None)
menu.add_cascade(label="File")
menu.add_command(label="Exit", command=Exit)
root['menu'] = menu
window = PanedWindow(root, orient=HORIZONTAL)
window.pack(fill="both")
left_menu = ModeMenu(window)
main_area = Frame(window, relief=SUNKEN)
window.add(main_area)
app = MainMenu(main_area)
root.mainloop()
