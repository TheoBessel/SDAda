with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body TH is

    function Hash (Cle : T_Cle) return Positive is
        V_Hash : Integer := F_Hash(Cle) mod V_Capacity;
    begin
        if V_Hash = 0 then
			V_Hash := 11;
		else
			Null;
		end if;
        return V_Hash;
    end Hash;


    procedure Initialiser (Sda: out T_TH) is
	begin
		for I in 1..V_Capacity loop
            LCA_Var.Initialiser(Sda(I));
        end loop;
	end Initialiser;


    procedure Detruire (Sda: in out T_TH) is
	begin
		for I in 1..V_Capacity loop
            LCA_Var.Detruire(Sda(I));
        end loop;
	end Detruire;


    function Est_Vide (Sda: T_TH) return Boolean is
        subtype T_Capacity is Integer range 1..V_Capacity;
        Vide : Boolean := True;
        I : T_Capacity := 1;
	begin
		loop
            Vide := LCA_Var.Est_Vide(Sda(I));
            I := I + 1;
            exit when I = V_Capacity or not Vide;
        end loop;
        return Vide;
	end Est_Vide;


    function Taille (Sda: in T_TH) return Natural is
        Taille : Natural := 0;
	begin
        for I in 1..V_Capacity loop
            Taille := Taille + LCA_Var.Taille(Sda(I));
        end loop;
		return Taille;
	end Taille;


    procedure Enregistrer (Sda : in out T_TH ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		LCA_Var.Enregistrer(Sda(Hash(Cle)), Cle, Valeur);
	end Enregistrer;


    procedure Supprimer (Sda : in out T_TH ; Cle : in T_Cle) is
	begin
		LCA_Var.Supprimer(Sda(Hash(Cle)), Cle);
	end Supprimer;


    function Cle_Presente (Sda : in T_TH ; Cle : in T_Cle) return Boolean is
	begin
		return LCA_Var.Cle_Presente(Sda(Hash(Cle)), Cle);
	end Cle_Presente;


    function La_Valeur (Sda : in T_TH ; Cle : in T_Cle) return T_Valeur is
	begin
		return LCA_Var.La_Valeur(Sda(Hash(Cle)), Cle);
	end La_Valeur;


    procedure Pour_Chaque (Sda : in T_TH) is
        procedure Traiter is new LCA_Var.Pour_Chaque(Traiter);
	begin
		for I in 1..V_Capacity loop
            Traiter(Sda(I));
        end loop;
	end Pour_Chaque;


    procedure Afficher_Debug (Sda: in T_TH) is
        procedure Afficher is new LCA_Var.Afficher_Debug(Afficher_Cle, Afficher_Donnee);
        Spaces : Unbounded_String;
	begin
		for I in 1..V_Capacity loop
            Spaces := (Length(To_Unbounded_String(Integer'Image(V_Capacity))) - Length(To_Unbounded_String(Integer'Image(I)))) * " ";
            Put(Integer'Image(I)); Put(To_String(Spaces));
            Put(" : ");
            Afficher(Sda(I));
            New_Line;
        end loop;
	end Afficher_Debug;

end TH;