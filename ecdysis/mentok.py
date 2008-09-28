import wx

class MentokApp(wx.App):
    def OnInit(self):
        self.frame = wx.Frame(None)
        return True



app = MentokApp() 
frame = wx.Frame(None, title="Hello World") 
frame.Show() 
app.MainLoop()


#if __name__=='__main__':
#    app = MentokApp()
#    app.MainLoop()
    
    
    
