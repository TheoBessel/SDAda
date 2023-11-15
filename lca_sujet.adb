with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with LCA;

procedure LCA_Sujet is
    package LCA_Int_Float is
        new LCA (T_Cle => Unbounded_String, T_Valeur => Integer);
    use LCA_Int_Float;

    Sda : T_LCA;

    procedure Afficher (S : in Unbounded_String; N : Integer) is
	begin
		Put ('"' & To_String (S) & '"');
        Put(" : ");
        Put (N, 1);
        New_Line;
	end Afficher;

	procedure Afficher is
		new Pour_Chaque(Afficher);

begin
    Initialiser(Sda);
    Enregistrer(Sda, To_Unbounded_String("un"), 1);
    Enregistrer(Sda, To_Unbounded_String("deux"), 2);
    Afficher(Sda); New_Line;
    Detruire(Sda);
end LCA_Sujet;