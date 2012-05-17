// Para compilar este modulo como DLL:
//
//                  mcs /t:library gailalib.cs
//
// Para ligar el DLL con algun programa escrito en C#:
//
//                  mcs /r:gailalib.dll algunprogram.cs


using System.Collections;

namespace Nyota {
    
    using System;

    public class Utils {

        public static void WrInt(int i) {
            Console.Write(i);
        }
	
        public static void WrReal(double r) {
            Console.Write(r);
        }
        
        public static void WrStr(string s) {
            Console.Write(s);
        }
        
        public static void WrBool(bool b) {
            Console.Write(b);
        }
        
        public static void WrLn() {
            Console.WriteLine();
        }
	
        public static int RdInt() {
            try {
                return int.Parse(Console.ReadLine());
            } catch(Exception ex) {
                Console.WriteLine(ex.Message);
                Console.WriteLine("Ese no es un integer, intentalo de nuevo: ");
                return RdInt();
            }
        }
	
        public static double RdReal() {
            try {
                return double.Parse(Console.ReadLine());
            } catch(Exception ex) {
                Console.WriteLine(ex.Message);
                Console.WriteLine("Ese no es un real, intentalo de nuevo: ");
                return RdReal();
            }
        }
        
        public static string RdStr() {
            return Console.ReadLine();
        }
        
        public static string AtStr(string s, int i) {
            return s[i]+"";
        }
        
        public static int LenStr(string s) {
            return s.Length;
        }
        
        public static int CmpStr(string s1, string s2) {
            /*if s1==s2 
                0;
            else if s1 > s2
                1;
            else if s1 < s2
                -1;*/
            return String.Compare(s1, s2);
        }
        
        public static string CatStr(string s1, string s2) {
            return s1+s2;
        }
        
        public static double IntToReal(int i) {
            return (double) i;
        }
        
        public static int RealToInt(double r) {
            return (int) r;
        }
        
        public static string IntToStr(int i) {
            return i.ToString();
        }
        
        public static int StrToInt(string s) {
            return int.Parse(s);
        }
        
        public static string RealToStr(double r) {
            return r.ToString();
        }
        
        public static double StrToReal(string s) {
            return double.Parse(s);
        }
	
		// biblioteca extendida
		public static void Fibonacci(int num) {
			for(int i=0; i<num; i++) {
				Console.WriteLine(NthF(i)+" ");
			}
		}
		
		public static int NthF(int Nth) {
			double Bphi = (1 + Math.Sqrt(5)) / 2; // 1.61803;
			double Sphi = (1 - Math.Sqrt(5)) / 2; // -0.618036;
			return (int) (( Math.Pow(Bphi, Nth) - Math.Pow(Sphi, Nth)) / Math.Sqrt(5));
		}
		
		public static void Nones(int start, int end) {
			for(; start<end; start++) {
				if(!(start%2==0)) {
					Console.Write(start+" ");
				}
			}
			Console.WriteLine();
		}
		
		public static void Pares(int start, int end) {
			for(; start<end; start++) {
				if(start%2==0) {
					Console.Write(start+" ");
				}
			}
			Console.WriteLine();
		}
		
		public static int Factorial(int num) {
			int acc = 1;
			for(int i=1; i<num; i++) {
				acc = acc * i;
			}
			return acc;
		}
		
		public static int Binario(int num) {
			if (num <= 0) return 0;
			string resultado = "";
			while(num!=0) {
				int residuo = num%2;
				resultado = residuo+resultado;            
				num = num / 2;
			}
			return int.Parse(resultado);
		}
		
		public static void Pino(int n) {
			string res = "";
			for(int i=0; i<n; i++) {
				int numEspacios = 2;
				for(int k=0; k<numEspacios; k++) {
					res += " ";
				}
				for(int j=0; j<i; j++) {
					res+="*";
				}
				for(int k=0; k<numEspacios; k++) {
					res += " ";
				}
				res += "\n";
			}
			Console.WriteLine(res);
		}
		
		public static void GB(int gb) {
			Console.WriteLine(gb+" GB = "+(gb*1024)+" MB");
			Console.WriteLine(gb+" GB = "+(gb*1024*1024)+" KB");
			Console.WriteLine(gb+" GB = "+(gb*1024*1024*1024)+" B");
			Console.WriteLine(gb+" GB = "+(gb*1024*1024*1024*8)+" b");
		}
		
		public static string FraseDelDia() {
			string[] frases = {
				"No todo lo que brilla es oro",
				"Más vale pajaro en mano, que ver un ciento volar",
				"Más vale tarde que nunca",
				"Agua que no has de beber dejala correr",
				"No hay mal que por bien no venga",
				"Las penas con pan son menos",
				"El diablo sabe mas por viejo que por diablo",
				"Si la montana no va a Mahoma, Mahoma va a la montana"
			};
			Random r = new Random();
			return frases[r.Next(frases.Length)];
		}
		
		public static int IntPow(int x, int y){
            return (int) Math.Pow(x,y);	
        }

        public static double RealPow(double x, double y){
            return Math.Pow(x,y);
        }
        
        public static double Floor(double x){
            return Math.Floor(x);
        }
        
        public static double Ceil(double x){
            return Math.Ceiling(x);
        }
        
        public static string Time(){
         return DateTime.Now.ToString("T");     
        }
        
        public static string Date(){
         return DateTime.Now.ToString("D");     
        }
        
        public static double SqrtInt(int x ){
               return Math.Sqrt(x);
        }
        
        public static double SqrtReal(double x){
               return Math.Sqrt(x);
        }
        
        public static double AbsInt(int x){
            return Math.Abs(x);
        }
        
        public static double AbsReal(double x){
            return Math.Abs(x);
        }
        
        public static int RandInt(){
            Random r = new Random( );
            return r.Next();
        }
        
        public static int RandIntSeed(int x){
            Random r = new Random(x);
            return r.Next();
        }
        
        public static double RandReal(){
            Random r = new Random( );
            return r.NextDouble();
        }
        
        public static double RandRealSeed(int x){
            Random r = new Random(x);
            return r.NextDouble();
        }
        
        public static string Rot13(string cadena){
            string rot13 = "";
            
            for (int i = 0; i < cadena.Length; i++){
            
                char c = cadena[i];
                int character = (int)c;
                
                if (! Char.IsLetter(c)){
                    rot13+= c+"";
                    continue;
                }
                
                if( (Char.IsLower(c) && character >= 'm')|| (Char.IsUpper(c) && character >= 'M') ){
                        character -= 13;
                }
                else{
                        character += 13;
                }
                rot13+= (char)character+"";
            }
            return rot13;
        }
	
    }
}

