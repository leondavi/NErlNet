import tkinter as tk
from tkinter import filedialog
from tkinter import messagebox
import pandas as pd

root = tk.Tk()

canvas1 = tk.Canvas(root, width=300, height=300, bg='#4b545f', relief='raised')
canvas1.pack()

label1 = tk.Label(root, text='Convert To CSV', bg='#4b545f', fg='white')
label1.config(font=('helvetica', 20))
canvas1.create_window(150, 60, window=label1)


def getTxt():
  global read_file
  import_file_path = filedialog.askopenfilename()
  read_file = pd.read_csv(import_file_path)


# # adding column headings
# read_file.columns = ['Name', 'Type']

browseButtonTxt = tk.Button(text="      Import Text File     ", command=getTxt, bg='green', fg='white',
                            font=('helvetica', 12, 'bold'))
canvas1.create_window(150, 130, window=browseButtonTxt)


def convertToCsv():
  global read_file

  export_file_path = filedialog.asksaveasfilename(defaultextension='.csv')
  read_file.to_csv(export_file_path, index=None)


saveAsButtonCsv = tk.Button(text='Convert Text to CSV', command=convertToCsv, bg='green', fg='white',
                            font=('helvetica', 12, 'bold'))
canvas1.create_window(150, 180, window=saveAsButtonCsv)


def exitApplication():
  MsgBox = tk.messagebox.askquestion('Exit Application', 'Are you sure you want to exit the application',
                                     icon='warning')
  if MsgBox == 'yes':
    root.destroy()


exitButton = tk.Button(root, text='       Exit       ', command=exitApplication, bg='brown', fg='white',
                       font=('helvetica', 12, 'bold'))
canvas1.create_window(150, 230, window=exitButton)

root.mainloop()
