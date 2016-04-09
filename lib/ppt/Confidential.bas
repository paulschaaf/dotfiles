Attribute VB_Name = "Confidential"
' **********************************************************************
' **********************************************************************
' Confidential
' **********************************************************************
' **********************************************************************
Public Const ConfidentialWatermarkName As String = "ConfidentialWatermark"

Sub MarkConfidential()
    ViewSlide
    AddConfidentialWaterMarkTo ThisSlide
End Sub

Public Function AllConfidentialSlideNumbers() As String
    Dim confidentials As String
    Dim newStr As String
    Dim aSlide As Slide
    
    For Each aSlide In ActivePresentation.Slides
        If IsConfidential(aSlide) Then
            If Len(confidentials) > 0 Then
                confidentials = confidentials + ", " + Str(aSlide.slideNumber)
            Else
                confidentials = Str(aSlide.slideNumber)
            End If
        End If
    Next aSlide
    AllConfidentialSlideNumbers = confidentials
End Function

Public Function AllConfidentialSlides() As Collection
    Dim confidentials As New Collection
    Dim aSlide As Slide
    
    For Each aSlide In ActivePresentation.Slides
        If IsConfidential(aSlide) Then
            confidentials.Add aSlide
        End If
    Next aSlide
    Set AllConfidentialSlides = confidentials
End Function

Public Function IsConfidential(aSlide As Slide) As Boolean
    On Error GoTo ErrorHandler
    Call ElementOnSlideWithIdentifier(aSlide, ConfidentialWatermarkName)
    IsConfidential = True
    Exit Function
    
ErrorHandler:
    IsConfidential = False
End Function

Public Function MasterConfidentialWatermark() As Shape
    Set MasterConfidentialWatermark = ThisSlideMaster.shapes(ConfidentialWatermarkName)
End Function

Public Sub AddConfidentialWaterMarkTo(aSlide As Slide)
    On Error GoTo ErrorHandler

    Dim aNewShape As Shape
    
    MasterConfidentialWatermark.Copy
    Set aNewShape = aSlide.shapes.Paste(1)
    With aNewShape
        .name = ConfidentialWatermarkName
        .Visible = True
        .Select
    End With
        
    Exit Sub

ErrorHandler:
    Select Case Err.Number
        Case 70  'Shape already exist, so delete the new one
            aNewShape.Delete
            Err.Clear
            Exit Sub
        Case Else
            On Error GoTo 0
    End Select
    
    Resume

End Sub

