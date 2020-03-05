unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, ChromeTabs,
  ChromeTabsClasses, ChromeTabsTypes,
  Vcl.StdCtrls, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxPC, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ChromeTabs1: TChromeTabs;
    Memo1: TMemo;
    Button1: TButton;
    cxPageControl1: TcxPageControl;
    Panel1: TPanel;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChromeTabs1ActiveTabChanged(Sender: TObject; ATab: TChromeTab);
    procedure ChromeTabs1ActiveTabChanging(Sender: TObject;
      AOldTab, ANewTab: TChromeTab; var Allow: Boolean);
    procedure ChromeTabs1ButtonAddClick(Sender: TObject; var Handled: Boolean);
    procedure ChromeTabs1ButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
      var Close: Boolean);
    procedure ChromeTabs1NeedDragImageControl(Sender: TObject; ATab: TChromeTab;
      var DragControl: TWinControl);
    procedure ChromeTabs1TabDragDrop(Sender: TObject; X, Y: Integer;
      DragTabObject: IDragTabObject; Cancelled: Boolean;
      var TabDropOptions: TTabDropOptions);
    procedure ChromeTabs1TabDragDropped(Sender: TObject; DragTabObject:
        IDragTabObject; NewTab: TChromeTab);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function AddTab(ChromeTabs: TChromeTabs; const Text: String;
      ImageIndex: Integer): TChromeTab;
    procedure Log(aTitle, aContent: string);
    procedure ProcessDroppedTab(Sender: TObject; X, Y: Integer;
      DragTabObject: IDragTabObject; Cancelled: Boolean;
      var TabDropOptions: TTabDropOptions);
  public
    { Public declarations }

    // taskbar icon application
    // -------------------------------------------------------------------------
    function GetAppID(AHandle: THandle): string;
    function SetAppID(AHandle: THandle; const AAppID: String): Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    // -------------------------------------------------------------------------
  end;

var
  Form1: TForm1;
  id : integer =0 ;

implementation

{$R *.dfm}

uses Unit2, ActiveX, PropSys, PropKey;

function TForm1.AddTab(ChromeTabs: TChromeTabs; const Text: String;
  ImageIndex: Integer): TChromeTab;
begin
  Result := ChromeTabs.Tabs.Add;

  Result.Caption := Text;
  Result.ImageIndex := ImageIndex;
  id := Result.ID;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  cxPageControl1.Pages[0].PageIndex := StrToInt(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.ChromeTabs1ActiveTabChanged(Sender: TObject; ATab: TChromeTab);
begin
//   Log((cxPageControl1.FindChildControl('a1') as TcxTabSheet).Caption, '');


  if ChromeTabs1.Tabs.Count = cxPageControl1.PageCount then
    cxPageControl1.ActivePageIndex := (cxPageControl1.FindChildControl('a' + InttoStr(ATab.ID)) as TcxTabSheet).TabIndex;
//    cxPageControl1.ActivePageIndex := ATab.Index;

  Text := cxPageControl1.ActivePage.Caption;
end;

procedure TForm1.ChromeTabs1ActiveTabChanging(Sender: TObject;
  AOldTab, ANewTab: TChromeTab; var Allow: Boolean);
begin
  // Log('ActiveTabChangingEvent', '[newTabIndex: ' + IntToStr(ANewTab.Index) + ']');
  // cxPageControl1.ActivePageIndex := ANewTab.Index;
end;

procedure TForm1.ChromeTabs1ButtonAddClick(Sender: TObject;
  var Handled: Boolean);
var
  sTabTitle: string;
  tabSheet: TcxTabSheet;
  currentCount: Integer;
  NewForm2: TForm2;
  NewForm1: TForm1;
begin
  // get current chrome tabs index
  currentCount := (Sender as TChromeTabs).Tabs.Count;

  // Log
  Log('ButtonAddClickEvent', '[TabIndex: ' + IntToStr(currentCount) + ']');

  // add new tab
  sTabTitle := 'Page' + IntToStr(currentCount);
  AddTab(ChromeTabs1, sTabTitle, -1);
  Handled := TRUE;

  // add new page
  tabSheet := TcxTabSheet.Create(Nil);
  with tabSheet do
  begin
    PageControl := cxPageControl1;
    Name := 'a' + inttostr(id);
    Caption := 'a' + inttostr(id);
    // TabVisible := False;
  end;
  cxPageControl1.ActivePageIndex := currentCount;

  // change form title
  Text := tabSheet.Caption;

  // create new form, embedded to page control
  NewForm2 := TForm2.Create(Application);
  NewForm2.Parent := tabSheet;
  NewForm2.Align := TAlign.alClient;
  NewForm2.BorderStyle := TFormBorderStyle.bsNone;
  NewForm2.Button1.Caption := 'button' + IntToStr(currentCount);
  NewForm2.Show;

end;

procedure TForm1.ChromeTabs1ButtonCloseTabClick(Sender: TObject;
  ATab: TChromeTab; var Close: Boolean);
begin
  Log('ButtonCloseTabClickEvent', 'TabIndex' + IntToStr(ATab.Index));
  // Log('Active page tab index', IntToStr(cxPageControl1.ActivePageIndex));
  // Log('Active page tab count', IntToStr(cxPageControl1.PageCount));

  // free closed page
  cxPageControl1.Pages[ATab.Index].Free;

  // set active page index to new page
  // cxPageControl1.ActivePageIndex := ChromeTabs1.ActiveTabIndex - 1;

end;

procedure TForm1.ChromeTabs1NeedDragImageControl(Sender: TObject;
  ATab: TChromeTab; var DragControl: TWinControl);
begin
  // set drag image
  DragControl := cxPageControl1.Pages[ATab.Index];
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  page: Integer;
begin
  for page := 0 to cxPageControl1.PageCount - 1 do
  begin
    cxPageControl1.Pages[page].TabVisible := false;
  end;

  cxPageControl1.ActivePageIndex := -1;
end;

procedure TForm1.Log(aTitle, aContent: string);
begin
  Memo1.Lines.Add(aTitle);
  Memo1.Lines.Add(aContent);
  Memo1.Lines.Add('');
end;

procedure TForm1.ProcessDroppedTab(Sender: TObject; X, Y: Integer;
  DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
var
  WinX, WinY: Integer;
  NewForm: TForm1;
  tabSheet: TcxTabSheet;
begin
  if (not Cancelled) and (DragTabObject.SourceControl <>
    DragTabObject.DockControl) and (DragTabObject.DockControl = nil) then
  begin
    // find the drop position
    WinX := MOuse.CursorPos.X - DragTabObject.DragCursorOffset.X -
      ((Width - ClientWidth) div 2);
    WinY := MOuse.CursorPos.Y - DragTabObject.DragCursorOffset.Y -
      (Height - ClientHeight) + ((Width - ClientWidth) div 2);

    // create new form
    {$region 'Create New Form'}
    NewForm := TForm1.Create(Application);
    NewForm.Position := poDesigned;
    NewForm.Left := WinX;
    NewForm.Top := WinY;
    SetAppID(NewForm.Handle, 'test1');

    NewForm.AddTab(NewForm.ChromeTabs1, ChromeTabs1.ActiveTab.Caption, -1);
    NewForm.Show;

    tabSheet := cxPageControl1.ActivePage;
    tabSheet.PageControl := NewForm.cxPageControl1;
    tabSheet.Name := 'a' + inttostr(id);
//    Log('tabsheet', tabSheet.Caption);


    {$endregion}

    Log(IntToStr(cxPageControl1.ActivePageIndex), IntToStr(NewForm.cxPageControl1.ActivePageIndex));

    // Remove the original tab
    TabDropOptions := [tdDeleteDraggedTab];
      Log('here',  '');

    // free closed page
//    cxPageControl1.Pages[cxPageControl1.ActivePageIndex].Free;
  end;

end;

procedure TForm1.ChromeTabs1TabDragDrop(Sender: TObject; X, Y: Integer;
  DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
begin
//  Log('TabDragDropEvent', '[TabIndex: ' + IntToStr((Sender as TChromeTab).Index) + ']');
  ProcessDroppedTab(Sender, X, Y, DragTabObject, Cancelled, TabDropOptions);
end;

// taskbar icon application
// ------------------------------------------------------------------------------
function SHGetPropertyStoreForWindow(hwnd: hwnd; const riid: TGUID;
  out ppv: IPropertyStore): HRESULT; stdcall; external 'shell32.dll';

procedure TForm1.ChromeTabs1TabDragDropped(Sender: TObject; DragTabObject:
    IDragTabObject; NewTab: TChromeTab);
var Tabs: TChromeTabs;
    tabSheet: TcxTabSheet;
begin
//  Tabs := Sender as TChromeTabs;
//  if ChromeTabs1.Tabs.Count <> cxPageControl1.PageCount then begin
//

//    tabSheet := cxPageControl1.ActivePage;
//    tabSheet.PageControl := Form1.cxPageControl1;
//    tabSheet.Name := 'a' + inttostr(id);
//
//    cxPageControl1.ActivePageIndex := cxPageControl1.PageCount -1 ;
//  end;
  Log('a', 'a');
end;

function TForm1.GetAppID(AHandle: THandle): string;
var
  hr: HRESULT;
  pps: IPropertyStore;
  v: TPropVariant;
begin
  hr := SHGetPropertyStoreForWindow(AHandle, IID_IPropertyStore, pps);
  if Succeeded(hr) then
  begin
    pps.GetValue(PKEY_AppUserModel_ID, v);
    Result := v.bstrVal;
  end
  else
    Result := '';
end;

function TForm1.SetAppID(AHandle: THandle; const AAppID: String): Boolean;
var
  hr: HRESULT;
  pps: IPropertyStore;
  v: TPropVariant;
begin
  hr := SHGetPropertyStoreForWindow(AHandle, IID_IPropertyStore, pps);
  if Succeeded(hr) then
  begin
    v.vt := VT_BSTR;
    v.bstrVal := SysAllocString(PChar(AAppID));
    Result := pps.SetValue(PKEY_AppUserModel_ID, v) = S_OK;
  end
  else
    Result := false;
end;

procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := HWND_DESKTOP
end;
// ------------------------------------------------------------------------------

end.
