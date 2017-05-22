Public Class Form1

    Public m As Integer（) = New Integer(13) {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14}
    Public score As Integer() = New Integer(13) {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    Public id As String() = New String(13) {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"}
    Public num As Integer = 10
    Public state As Integer = 1

    Public Sub ts(a As Object)
        gylabel.Text = ""
        For x = 0 To 13
            gylabel.Text += CStr(a(x))
            gylabel.Text += ","
        Next
    End Sub

    Public Sub FreshTeam()
        Dim x As Control
        Dim y As Integer = 0
        For a = 1 To num / 2
            For Each x In Panel1.Controls
                If x.TabIndex = a Then
                    x.Text = id(m(y) - 1)
                End If
            Next
            y += 1
        Next
        For a = 1 To num / 2
            For Each x In Panel2.Controls
                If x.TabIndex = a Then
                    x.Text = id(m(y) - 1)
                End If
            Next
            y += 1
        Next
    End Sub

    Public Sub FreshScroe()
        Dim x As Control
        Dim y As Integer = 0
        For a = 1 To num / 2
            For Each x In Panel3.Controls
                If x.TabIndex = a Then
                    x.Text = CStr(score(m(y) - 1))
                    y += 1
                End If
            Next
        Next
        For a = 1 To num / 2
            For Each x In Panel4.Controls
                If x.TabIndex = a Then
                    x.Text = CStr(score(m(y) - 1))
                    y += 1
                Else
                    Continue For
                End If
            Next
        Next
    End Sub

    Public Sub giveid()
        Dim x As Control
        Dim y As Integer = 0
        For Each x In Panel5.Controls
            If TypeOf (x) Is TextBox Then
                If x.Text = vbNullString Then
                    Continue For
                ElseIf x.Text IsNot vbNullString Then
                    id(y) = x.Text
                    y += 1
                End If
            End If
        Next
    End Sub

    Public Sub getnum()
        Label3.Text = ""
        Label4.Text = ""
        m = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14}
        If IsNumeric(textnum.Text) Then
            If CInt(textnum.Text) > 14 Then
                MsgBox("我觉得咱们玩的不是一个游戏", vbOK, "【→_→】")
            ElseIf CInt(textnum.Text < 3) Then
                MsgBox("哇，，这么点人还分什么组嘛", vbOK, "【=。=】")
            ElseIf CInt(textnum.Text) Mod 2 <> 0 Then
                MsgBox("QwQ暂时不支持奇数分组", vbOK, "灰常抱歉")
            Else
                num = CInt(textnum.Text)
            End If
        Else
            MsgBox("为什么不好好输入一个数字呢", vbOK, "【QwQ】")
        End If
    End Sub

    Public Sub randteam()
        Dim r(13) As Integer
        Dim l As Integer
        Randomize(Now.Second)
        For x = 0 To 13
            Randomize(DateTime.Now.Millisecond)
            r(x) = CInt(Rnd())
        Next
        For x = 0 To num - 1
            For y = 0 To num - 1
                If r(x) < r(y) Then
                    l = r(x)
                    r(x) = r(y)
                    r(y) = l
                    l = m(x)
                    m(x) = m(y)
                    m(y) = l
                End If
            Next
        Next
    End Sub

    Public Sub sort(a As Integer, b As Integer)
        Dim l As Integer
        For x = a To b
            For y = a To b
                If m(x) < m(y) Then
                    l = m(x)
                    m(x) = m(y)
                    m(y) = l
                End If
            Next
        Next
    End Sub

    Public Sub prtm(a As Integer, b As Integer, i As Control)
        Dim l As Integer = 17
        For x = a To b
            If Len(i.Text) + Len(id(m(x) - 1)) > l Then
                i.Text += vbCrLf
                l += 17
            End If
            i.Text += id(m(x) - 1)
            i.Text += ","
        Next
        i.Text = Mid(i.Text, 1, Len(i.Text) - 1)
    End Sub

    Public Sub win(a As Integer)
        If a = 1 Then
            For x = 0 To (num / 2) - 1
                score(m(x) - 1) += 1
            Next
        ElseIf a = 2 Then
            For x = (num / 2) To num - 1
                score(m(x) - 1) += 1
            Next
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Label3.Text = ""
        Label4.Text = ""
        randteam()
        sort(0, num / 2 - 1)
        sort(num / 2, num - 1)
        prtm(0, num / 2 - 1, Label3)
        prtm(num / 2, num - 1, Label4)
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        FreshTeam()
        FreshScroe()
    End Sub

    Private Sub SerB_Click(sender As Object, e As EventArgs) Handles SerB.Click
        giveid()
    End Sub


    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        win(1)
        FreshScroe()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        win(2)
        FreshScroe()
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        getnum()
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        If state = 1 Then
            Button6.Text = "切换为说明"
            gylabel.Text = "笠笠笠让我加的logo" + vbCrLf + "滑稽脸"
            PictureBox1.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            Labelpx.Visible = True
            gytextbox1.Visible = False
            state = 0
        Else
            Labelpx.Visible = False
            PictureBox2.Visible = False
            PictureBox1.Visible = False
            PictureBox3.Visible = False
            gylabel.Text = "使用说明"
            gytextbox1.Visible = True
            Button6.Text = "切换为滑稽"
            gytextbox1.Font = New Font("微软雅黑", 10)
            gytextbox1.Select(0, 4)
            gytextbox1.SelectionFont = New Font("微软雅黑", 14)
            state = 1
        End If
    End Sub


    Private Sub textnum_TextChanged(sender As Object, e As KeyEventArgs) Handles textnum.KeyDown
        If e.KeyCode = 13 Then
            getnum()
        End If
    End Sub

    Private Sub textnum_focus(sender As Object, e As EventArgs) Handles textnum.GotFocus, textnum.Click
        textnum.SelectAll()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        gytextbox1.Font = New Font("微软雅黑", 10)
        gytextbox1.Select(0, 4)
        gytextbox1.SelectionFont = New Font("微软雅黑", 14)
    End Sub
End Class
