with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TH;

procedure TH_Sujet is

    function Hash (Cle : Unbounded_String) return Integer is
    begin
        return Length(Cle);
    end Hash;

	package TH_String_Integer is
		new TH (T_Cle => Unbounded_String, T_Valeur => Integer, V_Capacity => 11, F_Hash => Hash);
	use TH_String_Integer;

    Sda : T_TH;

    procedure Afficher (S : in Unbounded_String; N : Integer) is
	begin
		Put ('"' & To_String (S) & '"');
        Put(" : ");
        Put (N, 1);
        New_Line;
	end Afficher;

	procedure Afficher is
		new Pour_Chaque(Afficher);

    A : Boolean;
begin
    Initialiser(Sda);
    Enregistrer(Sda, To_Unbounded_String("un"), 1);
    Enregistrer(Sda, To_Unbounded_String("deux"), 2);
    Enregistrer(Sda, To_Unbounded_String("trois"), 3);
    Enregistrer(Sda, To_Unbounded_String("quatre"), 4);
    Enregistrer(Sda, To_Unbounded_String("cinq"), 5);
    Enregistrer(Sda, To_Unbounded_String("quatre-vingt-dix-neuf"), 99);
    Enregistrer(Sda, To_Unbounded_String("vingt-et-un"), 21);
    Afficher(Sda); New_Line;
    Detruire(Sda);
end TH_Sujet;