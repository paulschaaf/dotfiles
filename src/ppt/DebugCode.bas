Attribute VB_Name = "DebugCode"
Private Sub ShowMasterShapeTemplates()
    MasterInstructorNote.Visible = True
    'MasterCourseExclusions.Visible = True
    'MasterConfidentialWatermark.Visible = True
End Sub

Private Sub HideMasterShapeTemplates()
    MasterInstructorNote.Visible = False
    'MasterCourseExclusions.Visible = False
    'MasterConfidentialWatermark.Visible = False
End Sub

Private Sub ShowTextValue()
    'Call OkMsgBox(ModuleName, "The Module name is ...")
    Call OkMsgBox(ModuleNumberString, "The ModuleNumberString is ...")
End Sub

Public Sub OkMsgBox(message As String, title As String)
    Call MsgBox(message, vbOKOnly, title)
End Sub

Public Function BooleanMsgBox(aMessage As String, aTitle As String) As Boolean
    If (MsgBox(aMessage, vbYesNo, aTitle)) = vbYes Then
        BooleanMsgBox = True
    Else
        BooleanMsgBox = False
    End If
End Function

Private Sub SelectInstructorNote()
    On Error GoTo none
    InstructorNoteOn(ThisSlide).Select
    Exit Sub
none:
    Call MsgBox("This slide has no instructor note", 0, "SelectInstructorNote")
End Sub

Private Sub SelectOverhead()
    On Error GoTo none
    OverheadOn(ThisSlide).Select
    Exit Sub
none:
    Call MsgBox("This slide has no overhead", 0, "SelectOverhead")
End Sub

Private Sub SelectStudentNote()
    On Error GoTo none
    StudentNoteOn(ThisSlide).Select
    Exit Sub
none:
    Call MsgBox("This slide has no student note", 0, "SelectStudentNote")
End Sub

Private Sub ShowMasterElementsDeclaration()
    Dim measurements As String
    
    measurements = MeasurementsDeclarationFor(MasterOverhead)
    measurements = measurements + MeasurementsDeclarationFor(MasterStudentNote)
    measurements = measurements + MeasurementsDeclarationFor(MasterInstructorNote)
    
    InsertTextBox (measurements)
End Sub

Private Sub InspectAllShapes()
'
' Debugging use.
'
    Dim aShape As Shape
    
    For Each aShape In ThisSlide.Shapes
        aShape.Select
        InspectSelection
    Next aShape
End Sub

Private Sub InspectSelection()
'
' Debugging use.  Run this then check out the variables in a locals window.
'
    Dim aShape As Shape
    Dim id
    Dim anActiveWindow As DocumentWindow
    Dim aSelection As Selection
    Dim aShapeRange As ShapeRange
    Dim aPresentation As Presentation
    Dim aSlideMaster As Master
    Dim aNotesMaster As Master
    Dim aShapeType As MsoShapeType
    Dim aShapeName, height, width, top, left, version
    Dim aShapePlaceholderType As PpPlaceholderType
    
    Dim message As String
    Dim rotation As String
    Dim prefix As String
    
    On Error Resume Next
    
    If (UsingLandscape) Then
        rotation = "_Landscape"
    Else
        rotation = "_Portrait"
    End If
    
    Set aSlideMaster = ThisSlideMaster
    Set aNotesMaster = ThisNotesMaster
    Set aPresentation = ActivePresentation
    version = aPresentation.BuiltInDocumentProperties("Revision number")
    Set anActiveWindow = ActiveWindow
    Set aSelection = anActiveWindow.Selection
    Set aShapeRange = aSelection.ShapeRange
    Set aShape = aShapeRange(1)
    id = aShape.id
    aShapeName = aShape.name
    aShapeType = aShape.Type
    height = aShape.height:     width = aShape.width
    top = aShape.top:           left = aShape.left
    
    aShapePlaceholderType = aShape.PlaceholderFormat.Type
    
    ' stop execution so user can examine variables in the Locals window
    Stop
End Sub

Sub InsertTextBox(text As String)
    ActiveWindow.View.slide.Shapes.AddShape(msoShapeRectangle, 216#, 120#, 270#, 216#).Select
    With ActiveWindow.Selection.ShapeRange
        .Line.ForeColor.RGB = RGB(255, 0, 0)
        .Line.Visible = msoTrue
        .Line.Weight = 2.25
        .Line.Visible = msoTrue
        .Line.Style = msoLineSingle
        With .TextFrame
            .TextRange.ParagraphFormat.Alignment = ppAlignLeft
            .TextRange.text = text
            With .TextRange.font
                .name = "Times New Roman"
                .size = 11
                .Bold = msoFalse
                .Italic = msoFalse
                .Underline = msoFalse
                .Shadow = msoFalse
                .Emboss = msoFalse
                .BaselineOffset = 0
                .AutoRotateNumbers = msoFalse
                .color.SchemeColor = ppForeground
            End With
            .TextRange.Select
        End With
    End With
    ActiveWindow.Selection.ShapeRange.TextFrame.AutoSize = ppAutoSizeShapeToFitText
End Sub

Private Function GlobalVariableDeclaration(name As String, value As String) As String
    GlobalVariableDeclaration = "Public Const " + name + " = " + value + Chr$(CharCode:=13)
End Function

Private Sub ViewNotesMaster()
    ActiveWindow.ViewType = ppViewNotesMaster
End Sub

Private Sub ViewSlideMaster()
    ActiveWindow.ViewType = ppViewSlideMaster
End Sub

Private Sub SelectNotesMasterShapes()
'
' Debugging use.
'
    Dim aShape As Shape
    Dim counter
    
    ViewNotesMaster
    counter = 1
    For Each aShape In ThisNotesMaster.Shapes
        aShape.Select
        response = MsgBox("Shape #" + CStr(counter) + " is named '" + aShape.name + "'.", vbOKCancel)
        If response = vbCancel Then
            Exit Sub
        End If
        counter = counter + 1
    Next aShape

End Sub

Private Sub NameAllMasterShapesFromUser()
'
' Debugging use.
'
    Dim aShape As Shape
    Dim counter
    
    ViewNotesMaster
    counter = 1
    For Each aShape In ThisNotesMaster.Shapes
        aShape.Select
        NameSelectionFromUser
        counter = counter + 1
    Next aShape

    ViewSlideMaster
    counter = 1
    For Each aShape In ThisSlideMaster.Shapes
        aShape.Select
        NameSelectionFromUser
        counter = counter + 1
    Next aShape

End Sub

Function ActiveShapeRange() As ShapeRange
    Set ActiveShapeRange = ActiveWindow.Selection.ShapeRange
End Function

Private Sub NameSelection(aName As String)
'
' Development time only.  Name the selected item.  Useful if the
' Master page has changed.
'
On Error Resume Next
    ActiveShapeRange(1).name = aName
End Sub

Private Sub SetSelectionAsModuleNameBox()
'
' Development time only.  Run this if the Master page has changed.
' Go to the Master Notes view, select it, then run this.
'
    NameSelection ModuleNameBoxName
End Sub
Private Sub SetSelectionAsOverhead()
'
' Development time only.  Run this if the Master page has changed.
' Go to the Master Notes view, select it, then run this.
'
    NameSelection OverheadName
End Sub

Private Sub SetSelectionAsStudentNote()
'
' Development time only.  Run this if the Master page has changed.
' Go to the Master Notes view, select it, then run this.
'
    NameSelection StudentNoteName
End Sub

Private Sub SetSelectionAsInstructorNote()
'
' Development time only.  Run this if the Master page has changed.
' Go to the Master Notes view, select it, then run this.
'
    NameSelection InstructorNoteName
End Sub

Private Sub NameSelectionFromUser()
'
' Development time only.  Name the selected item.  Useful if the
' Master page has changed.
'
    NameSelection InputBox("What would you like to name the selection?", "Name Selection", ActiveShapeRange(1).name)
End Sub

Private Sub SelectMasterElements()
'
' Debug.  Selects each of the named items in turn on the master page.
'
    ViewNotesMaster
    MasterOverhead.Select
    MsgBox "Master Overhead"
    
    MasterStudentNote.Select
    MsgBox "Master Student Note"
    
    MasterInstructorNote.Select
    MsgBox "Master Instructor Note"
    
    ViewSlideMaster
    ModuleNameBox.Select
    MsgBox "Master Module Name Box"

End Sub

Private Function MeasurementsDeclarationFor(aShape As Shape) As String
'
' Debugging use.  Run this then check out the variables in a locals window.
'
    Dim prefix, rotation As String
    
    If (UsingLandscape) Then
        rotation = "_Landscape"
    Else
        rotation = "_Portrait"
    End If
    
    prefix = ""
    'If (aShape.Parent.ViewType = ppViewNotesMaster) Then
    If (aShape.id = MasterStudentNote.id) Or _
        (aShape.id = MasterOverhead.id) Or _
        (aShape.id = MasterInstructorNote.id) Then
        prefix = aShape.name
    End If

    message = GlobalVariableDeclaration(prefix + "Height" + rotation, aShape.height)
    message = message + GlobalVariableDeclaration(prefix + "Width" + rotation, aShape.width)
    message = message + GlobalVariableDeclaration(prefix + "Top" + rotation, aShape.top)
    message = message + GlobalVariableDeclaration(prefix + "Left" + rotation, aShape.left)
    MeasurementsDeclarationFor = message + Chr$(CharCode:=13)
End Function
