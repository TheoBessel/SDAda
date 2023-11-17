with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Exceptions;	   use Ada.Exceptions;
with SDA_Exceptions;       use SDA_Exceptions;
with Alea;

with TH;

-- Évaluer la qualité du générateur aléatoire et les TH.
procedure Evaluer_Alea_TH is


	-- Afficher l'usage.
	procedure Afficher_Usage is
	begin
		New_Line;
		Put_Line ("Usage : " & Command_Name & " Borne Taille");
		New_Line;
		Put_Line ("   Borne  : les nombres sont tirés dans l'intervalle 1..Borne");
		Put_Line ("   Taille : la taille de l'échantillon");
		New_Line;
	end Afficher_Usage;


	-- Afficher le Nom et la Valeur d'une variable.
	-- La Valeur est affichée sur la Largeur_Valeur précisée.
	procedure Afficher_Variable (Nom: String; Valeur: in Integer; Largeur_Valeur: in Integer := 1) is
	begin
		Put (Nom);
		Put (" : ");
		Put (Valeur, Largeur_Valeur);
		New_Line;
	end Afficher_Variable;


	Bad_Argument_Exception : Exception;
	-- Récupère les arguments donnés au programme.
	-- On vérifie le nombre et le format des arguments et on lève une exception si il y a une erreur
	procedure Recuperer_Arguments (Borne : out Integer; Taille : out Integer) is
	begin
		if (Argument_Count < 2) then
			raise Bad_Argument_Exception with "Le programme n'a pas assez d'arguments !";
		elsif (Argument_Count > 2) then
			raise Bad_Argument_Exception with "Le programme a trop d'arguments !";
		end if;
		begin
			Borne := Integer'Value(Argument(1));
			if Borne < 1 then
				raise Bad_Argument_Exception with " must be an integer greater than 1";
			end if;
		exception 
			when E : Bad_Argument_Exception => raise Bad_Argument_Exception with "Mauvais argument : " & '"' & Argument(1) & '"' & Exception_Message(E);
			when Constraint_Error => raise Bad_Argument_Exception with "Mauvais argument : " & '"' & Argument(1) & '"';
		end;
		begin
			Taille := Integer'Value(Argument(2));
			if Taille < 1 then
				raise Bad_Argument_Exception with " must be an integer greater than 1";
			end if;
		exception 
			when E : Bad_Argument_Exception => raise Bad_Argument_Exception with "Mauvais argument : " & '`' & Argument(2) & '`' & Exception_Message(E);
			when Constraint_Error => raise Bad_Argument_Exception with "Mauvais argument : " & '`' & Argument(2) & '`';
		end;
	end Recuperer_Arguments;

	-- Évaluer la qualité du générateur de nombre aléatoire Alea sur un
	-- intervalle donné en calculant les fréquences absolues minimales et
	-- maximales des entiers obtenus lors de plusieurs tirages aléatoires.
	--
	-- Paramètres :
	-- 	  Borne: in Entier	-- le nombre aléatoire est dans 1..Borne
	-- 	  Taille: in Entier -- nombre de tirages (taille de l'échantillon)
	-- 	  Min, Max: out Entier -- fréquence minimale et maximale
	--
	-- Nécessite :
	--    Borne > 1
	--    Taille > 1
	--
	-- Assure : -- poscondition peu intéressante !
	--    0 <= Min Et Min <= Taille
	--    0 <= Max Et Max <= Taille
	--    Min /= Max ==> Min + Max <= Taille
	--
	-- Remarque : On ne peut ni formaliser les 'vraies' postconditions,
	-- ni écrire de programme de test car on ne maîtrise par le générateur
	-- aléatoire.  Pour écrire un programme de test, on pourrait remplacer
	-- le générateur par un générateur qui fournit une séquence connue
	-- d'entiers et pour laquelle on pourrait déterminer les données
	-- statistiques demandées.
	-- Ici, pour tester on peut afficher les nombres aléatoires et refaire
	-- les calculs par ailleurs pour vérifier que le résultat produit est
	-- le bon.
	procedure Calculer_Statistiques (
		Borne    : in Integer;  -- Borne supérieur de l'intervalle de recherche
		Taille   : in Integer;  -- Taille de l'échantillon
		Min, Max : out Integer  -- min et max des fréquences de l'échantillon
	) with
		Pre => Borne > 1 and Taille > 1,
		Post => 0 <= Min and Min <= Taille
			and 0 <= Max and Max <= Taille
			and (if Min /= Max then Min + Max <= Taille)
	is
		package Mon_Alea is
			new Alea (1, Borne);
		use Mon_Alea;

		function Hash (Cle : in Integer) return Integer is
		begin
			return Cle;
		end Hash;

		package TH_Int_Freq is
			new TH (T_Cle => Integer, T_Valeur => Integer, V_Capacity => 1000, F_Hash => Hash);
		use TH_Int_Freq;

		Sda : T_TH;
		Cle_Alea : Integer;
		Freq_Alea: Integer;
	begin
		Initialiser(Sda);

		for I in 1..Taille loop
			Mon_Alea.Get_Random_Number(Cle_Alea);
			
			begin
				Freq_Alea := La_Valeur(Sda, Cle_Alea) + 1;
			exception
				when Cle_Absente_Exception => Freq_Alea := 1;
			end;

			Enregistrer(Sda, Cle_Alea, Freq_Alea);
		end loop;

		Min := Taille + 1;
		Max := -1;

		for I in 1..Taille loop
			begin
				Freq_Alea := La_Valeur(Sda, I);
			exception
				when Cle_Absente_Exception => Freq_Alea := 0;
			end;

			if Min > Freq_Alea then
				Min := Freq_Alea;
			elsif Max < Freq_Alea then
				Max := Freq_Alea;
			else
				Null;
			end if;
		end loop;
	end Calculer_Statistiques;

	Min, Max: Integer; -- fréquence minimale et maximale d'un échantillon
	Borne: Integer;    -- les nombres aléatoire sont tirés dans 1..Borne
	Taille: Integer;   -- nombre de tirages aléatoires
begin
	-- Récupérer les arguments de la ligne de commande
	begin
		Recuperer_Arguments(Borne, Taille); -- Récupère les arguments avec robustesse
		exception
			when E : Bad_Argument_Exception => Put_Line(Exception_Message(E)); Afficher_Usage; return;
	end;
	-- Afficher les valeur de Borne et Taille
	Afficher_Variable ("Borne ", Borne);
	Afficher_Variable ("Taille", Taille);

	Calculer_Statistiques (Borne, Taille, Min, Max);

	-- Afficher les fréquence Min et Max
	Afficher_Variable ("Min", Min);
	Afficher_Variable ("Max", Max);
end Evaluer_Alea_TH;
