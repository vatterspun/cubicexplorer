unit CE_TBActions;

interface

uses
  // TB2k, TBX, SpTBX
  TB2Item, SpTBXItem,
  // Tnt Control
  TntActnList,
  // System Units
  ActnList, Classes;

type

  TCEToolbarAction = class(TTntAction)
  private
    fCanExecute: Boolean;
  public
    ItemClass: TTBCustomItemClass;
    constructor Create(AOwner: TComponent); override;
  published
    property CanExecute: Boolean read fCanExecute write fCanExecute default true;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterActions('CubicExplorer', [TCEToolbarAction], nil);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarAction
-------------------------------------------------------------------------------}
constructor TCEToolbarAction.Create(AOwner: TComponent);
begin
  inherited;
  fCanExecute:= true;
end;

end.
