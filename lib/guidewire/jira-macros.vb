' -*- eval: (kill-ring-save (point-min) (point-max)) -*-
' Puts all code in clipboard when opening in Emacs.

Sub ScrollDisplayToTopLeft()
   Range("A1").Select
End Sub

Sub DeleteLogoAndUselessRows()
   ' Delete Guidewire logo
   ActiveSheet.DrawingObjects.Delete

   ' Protect against accidentally running this again
   On Error GoTo OnlyRunOnce

   Cells.Find(What:="Displaying issues", After:=ActiveCell, LookIn:=xlFormulas, _
      LookAt:=xlPart, SearchOrder:=xlByColumns, SearchDirection:=xlNext, _
      MatchCase:=False, SearchFormat:=False).Activate
   Rows("1:" + Format(Selection.Row + 1)).Delete Shift:=xlUp

   ' Delete final row
   Cells.Find(What:="Generated", After:=ActiveCell, LookIn:=xlFormulas, _
      LookAt:=xlPart, SearchOrder:=xlByColumns, SearchDirection:=xlNext, _
      MatchCase:=False, SearchFormat:=False).Activate
      Rows(Format(Selection.Row) + ":" + Format(Selection.Row + 50)).Delete Shift:=xlUp

   Call ScrollDisplayToTopLeft
OnlyRunOnce:

End Sub

Sub FixCellSizes()
   With Cells
      .EntireColumn.AutoFit
      .EntireRow.AutoFit
   End With
End Sub

Sub ChangeStatusesToOpenIn(aColumn)
   With Columns(aColumn)
      .Replace What:="In Progress", Replacement:="Open", LookAt:= _
         xlPart, SearchOrder:=xlByRows, MatchCase:=False, SearchFormat:=False, _
         ReplaceFormat:=False
      .Replace What:="Reopened", Replacement:="Open", LookAt:=xlPart, _
         SearchOrder:=xlByRows, MatchCase:=False, SearchFormat:=False, _
         ReplaceFormat:=False
   End With
End Sub

Sub Sort()
   Call ChangeStatusesToOpenIn("A:A")
   Selection.CurrentRegion.Sort Key1:=Range("A2"), Order1:=xlAscending, Key2:=Range _
      ("B2"), Order2:=xlAscending, Key3:=Range("C2"), Order3:=xlAscending, _
      Header:=xlGuess, OrderCustom:=1, MatchCase:=False, Orientation:= _
      xlTopToBottom, DataOption1:=xlSortNormal, DataOption2:=xlSortNormal, _
      DataOption3:=xlSortNormal
End Sub

Sub DuplicateWorksheet()
    Sheets(1).Copy After:=Sheets(1)
End Sub

Sub CleanUp()
   Dim LastRow

   ActiveWindow.Zoom = 75
   Call ScrollDisplayToTopLeft
   Selection.CurrentRegion.MergeCells = False
   Call DeleteLogoAndUselessRows
   Call FixCellSizes
   Call Sort

   Columns("A:B").Select
   With Selection
      .Copy
      .Insert Shift:=xlToRight
   End With

   LastRow = Selection.SpecialCells(xlCellTypeLastCell).Row
   Range("A2").FormulaR1C1 = "=IF(RC[2]=R[-1]C[2],"""",RC[2])"
   Range("B2").FormulaR1C1 = "=IF(AND(RC[-1]="""",RC[2]=R[-1]C[2]),"""",RC[2])"
   Range("A2:B2").AutoFill Destination:=Range("A2:B" + Format(LastRow)), Type:=xlFillDefault

   Range("C:D,I:I,F:F").EntireColumn.Hidden = True
   With Selection.CurrentRegion
      .Select
      .Copy
   End With
End Sub

Sub RunMacrosOnDuplicateWorksheet()
   ' Put cursor here & press <F5> to test the macros
   ' or just run on the duplicate so you have a backup
   ActiveWindow.Zoom = 75
   Call DuplicateWorksheet
   Sheets(2).CleanUp
End Sub
