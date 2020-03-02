unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, ChromeTabs, ChromeTabsClasses,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ChromeTabs1: TChromeTabs;
    PageControl1: TPageControl;
    Memo1: TMemo;
    procedure ChromeTabs1ActiveTabChanged(Sender: TObject; ATab: TChromeTab);
    procedure ChromeTabs1ActiveTabChanging(Sender: TObject; AOldTab, ANewTab:
        TChromeTab; var Allow: Boolean);
    procedure ChromeTabs1ButtonAddClick(Sender: TObject; var Handled: Boolean);
    procedure ChromeTabs1ButtonCloseTabClick(Sender: TObject; ATab: TChromeTab; var
        Close: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function AddTab(ChromeTabs: TChromeTabs; const Text: String; ImageIndex: Integer): TChromeTab;
    procedure Log(aTitle, aContent: string);
  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2;

function TForm1.AddTab(ChromeTabs: TChromeTabs; const Text: String;
  ImageIndex: Integer): TChromeTab;
begin
  Result := ChromeTabs.Tabs.Add;

  Result.Caption := Text;
  Result.ImageIndex := ImageIndex;
end;

procedure TForm1.ChromeTabs1ActiveTabChanged(Sender: TObject; ATab: TChromeTab);
begin
//  Log('Event', 'changed' + IntToStr(ATab.Index));
//  PageControl1.ActivePageIndex := ATab.Index;
end;

procedure TForm1.ChromeTabs1ActiveTabChanging(Sender: TObject; AOldTab,
    ANewTab: TChromeTab; var Allow: Boolean);
begin
//  Log('Event', 'Changing, old: ' + IntToStr(AOldTab.Index) + ', new: ' + IntToStr(ANewTab.Index));
  PageControl1.ActivePageIndex := ANewTab.Index;
end;

procedure TForm1.ChromeTabs1ButtonAddClick(Sender: TObject; var Handled:
    Boolean);
var sTabTitle: string;
    tabSheet : TTabSheet;
    currentCount : integer;
    NewForm2 : TForm2;
    NewForm1 : TForm1;
begin
  // get current chrome tabs index
  currentCount := (Sender as TChromeTabs).Tabs.Count;

  // log
  Log('Event', 'Add Tab');

  // add new tab
  sTabTitle := 'New' + IntToStr(currentCount);
  AddTab(ChromeTabs1, sTabTitle, -1);
  Handled := TRUE;

  // add new page
  tabSheet := TTabSheet.Create(Nil);
  with tabSheet do begin
    PageControl := PageControl1;
    Name := 'page' + IntToStr(currentCount);
    caption := 'page' + IntToStr(currentCount);
    TabVisible := False;
  end;
  PageControl1.ActivePageIndex := currentCount;

  // create new form, embedded to page control
  NewForm2 := TForm2.Create(Application);
  NewForm2.Parent := tabSheet;
  NewForm2.Align := TAlign.alClient;
  NewForm2.BorderStyle := TFormBorderStyle.bsNone;
  NewForm2.Button1.Caption := 'button' + IntToStr(currentCount);
  NewForm2.Show;

  // change form title
  Text := 'title' + IntToStr(currentCount);
end;

procedure TForm1.ChromeTabs1ButtonCloseTabClick(Sender: TObject; ATab:
    TChromeTab; var Close: Boolean);
begin
  Log('TabIndex', IntToStr(ATab.Index));
//  Log('Active page tab index', IntToStr(PageControl1.ActivePageIndex));
//  Log('Active page tab count', IntToStr(PageControl1.PageCount));

  // free closed page
  PageControl1.Pages[ATab.Index].Free;

  // set active page inde to new page
  PageControl1.ActivePageIndex := ChromeTabs1.ActiveTabIndex - 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
var page : integer;
begin
  for page := 0 to PageControl1.PageCount - 1 do begin
    PageControl1.Pages[page].TabVisible := false;
  end;

  PageControl1.ActivePageIndex := -1;
end;

procedure TForm1.Log(aTitle, aContent: string);
begin
  Memo1.Lines.Add(aTitle);
  Memo1.Lines.Add(aContent);
  Memo1.Lines.Add('');
end;

end.
