Attribute VB_Name = "ThumbTabs"
' **********************************************************************
' **********************************************************************
' Thumb Tabs
' **********************************************************************
' **********************************************************************

' Not user-modifiable
Public Const ThumbTabsSlotsPerPortraitPage = 6
Public Const ThumbTabOffsetFromRightEdgeOfSlide = 7

Public Const ThumbTabName As String = "ThumbTab"


Public Function ThumbTabOn(aSlide As Slide) As Shape
    Set ThumbTabOn = ElementOnNotesPageWithIdentifier(aSlide, ThumbTabName)
End Function

Sub PositionThumbTab(aShape As Shape)
    If (developerMode) Then
        aShape.Delete
    Else
        Const topOffset = 10
        Const minTabWidth = 15
        Dim slotHeight, slotNumber, slotTop, pageHeight, pageWidth, rightMarginStart, tabLeft, tabWidth
    
        pageHeight = ActivePresentation.PageSetup.slideWidth - topOffset
        pageWidth = ActivePresentation.PageSetup.slideHeight
        rightMarginStart = MasterOverhead.left + MasterOverhead.width
    
        If isSection Then
            slotNumber = 0
            slotHeight = pageHeight
        Else
            slotNumber = (ModuleNumber - 1) Mod ThumbTabsSlotsPerPortraitPage
            slotHeight = pageHeight / ThumbTabsSlotsPerPortraitPage
        End If
    
        tabLeft = rightMarginStart + ThumbTabOffsetFromRightEdgeOfSlide
        tabWidth = pageWidth - tabLeft
        If tabWidth < minTabWidth Then
            tabWidth = minTabWidth
        End If
        slotTop = (slotHeight * slotNumber) + topOffset
    
        With aShape
            .height = slotHeight
            .left = tabLeft
            .width = tabWidth
            .top = slotTop
        End With
    End If
End Sub

Private Sub RemoveThumbTabs()
    Dim aSlide As Slide
    
    On Error Resume Next
    For Each aSlide In ActivePresentation.Slides
        ThumbTabOn(aSlide).Delete
    Next
End Sub

Private Sub AddThumbTabs()
    Dim isld As Integer
    Dim aModuleNumberString As String
    Dim slideNumber As Integer
    Dim aSlide As Slide
    Dim thumbTab As Shape
    
    If isSection() Then
        aModuleNumberString = SectionName
    Else
        aModuleNumberString = ModuleNumberFormalString
    End If
    
    slideNumber = 1
    For Each aSlide In ActivePresentation.Slides

        ' on every odd slide
        If isOdd(slideNumber) Then
            Set thumbTab = NotesPageShapesOn(aSlide).AddTextbox(msoTextOrientationHorizontal, 0, 0, 0, 0)
                        
            PositionThumbTab thumbTab
            With thumbTab
                .name = ThumbTabName
                .LockAspectRatio = False
                With .Fill
                    .Visible = True
                    .Solid
                    .ForeColor.SchemeColor = ppShadow
                End With
                With .TextFrame
                    .MarginTop = 0#
                    .MarginBottom = 0#
                    .MarginLeft = 0#
                    .MarginRight = 0#
                    .WordWrap = False
                    .VerticalAnchor = msoAnchorTop
                    .HorizontalAnchor = msoAnchorCenter
                    .Orientation = msoTextOrientationVerticalFarEast
                    With .TextRange
                        .text = aModuleNumberString
                        .Paragraphs.ParagraphFormat.Bullet.Visible = False
                        With .Font
                            .name = "Arial"
                            .size = 12
                            .Bold = True
                            .color.SchemeColor = ppBackground
                        End With
                    End With
                End With
                .rotation = 180
            End With
        End If
        slideNumber = slideNumber + 1
    Next
End Sub


