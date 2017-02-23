unit CE_ToolbarReg;

interface

uses
  Classes;

procedure Register;

{==============================================================================}
implementation

uses
  CE_Toolbar, CE_ToolbarEditorItems, CE_SpTBXItems,
  TB2DsgnItemEditor;

procedure Register;
begin
  // Register toolbar
  RegisterComponents('CubicExplorer', [TCEToolbar]);

  // Register Dynamic Spacer
  RegisterNoIcon([TCEToolbarDynamicSpacerItem]);
  RegisterClasses([TCEToolbarDynamicSpacerItem]);
  TBRegisterItemClass(TCEToolbarDynamicSpacerItem, 'New CE Dynamic Spacer', HInstance);

  // Register Fixed Spacer
  RegisterNoIcon([TCEToolbarFixedSpacerItem]);
  RegisterClasses([TCEToolbarFixedSpacerItem]);
  TBRegisterItemClass(TCEToolbarFixedSpacerItem, 'New CE Fixed Spacer', HInstance);

  // Register Stretcher
  RegisterNoIcon([TCEToolbarStretcherItem]);
  RegisterClasses([TCEToolbarStretcherItem]);
  TBRegisterItemClass(TCEToolbarStretcherItem, 'New CE Stretcher', HInstance);

  // Register EditItem
  RegisterNoIcon([TCEToolbarEditItem]);
  RegisterClasses([TCEToolbarEditItem]);
  TBRegisterItemClass(TCEToolbarEditItem, 'New CE Edit Item', HInstance);

  // Register ComboBoxItem
  RegisterNoIcon([TCEToolbarComboBoxItem]);
  RegisterClasses([TCEToolbarComboBoxItem]);
  TBRegisterItemClass(TCEToolbarComboBoxItem, 'New CE ComboBox Item', HInstance);

  // Register TCETrackBar
  RegisterComponents('CubicExplorer', [TCETrackBar]);
end;

end.
