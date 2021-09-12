#include"finalhashtable.h"
int flag=1;
string label,opcode,operand[10];

string decTobin(int n)
{
    string s="";
    int k;
    for (int i = 3; i >= 0; i--) {
         k = n >> i;
        if (k & 1) s+='1';
        else s+='0'; }
        if(n>>4 !=0) s="INF";
        return s;
}
bool fun(string ff)
{
    string d[10]={};
    char gfg[100]={};
    strcpy(gfg,ff.c_str());
    char* tok;
    tok = strtok(gfg, ":");
    string h=tok;
    int i=0;
    if(h.size()==ff.size()) {i++;label="";}
    else label=h;
    while (tok != 0) {
    d[i++]=tok;
    tok = strtok(0, ":");
    }
    gfg[100]={};
    strcpy(gfg,d[i-1].c_str());
    i--;
    tok = strtok(gfg, " ");
    while (tok != 0) {
    d[i++]=tok;
    tok = strtok(0, " ");
    }
    for(int j=0;j<i;j++){
        for(int k=0;k<d[j].size();k++){
            if(d[j][k]==',') d[j].resize(d[j].size()-1);
        }
    }
    opcode=d[1];
    for(int j=0;j<3;j++){
        if(d[j+2][0]=='#') {
                d[j+2].erase(d[j+2].begin()+0);
                int g=0;
                stringstream st(d[j+2]);
                st>>g;
                if(decTobin(g)!="INF")
                operand[j]=decTobin(g);
                else return 0;
        }
      else   operand[j]=d[j+2];
    }
    return 1;
}

template<typename K, typename V>
class OpcodeTable : public HashTable<K,V> , public HTWrapper<K,V> {
    public:
    string serialize(OpcodeTable<K,V> h)
    {
        string s = "";
        for (int i=0; i<10; i++)
        {
            HashNode<K,V>* ptr=h.record[i];
            while (ptr !=  NULL)
            {
                int ln = (ptr->key).length();
                s.push_back('0' + ln);
                s = s + "~" + ptr->key ;
                const int u=0;
                int *pt=&u;
                for(int j=0;j<3;j++)
                {
                    *pt=j;
                    int w = get<u>((ptr->object)[0]);
                    stringstream s1;
                    s1<<w;
                    string ww=s1.str();
                    ln = ww.length();
                    s.push_back('0' + ln);
                    s = s + "~" + ww;
                }
                ptr=ptr->next;
            }
        }
        return s;
    }
    void saveToFile(OpcodeTable<K,V> h)
    {   string filename;
        cout<<"Enter filenames: ";
        cin>>filename;
        ofstream out(filename.c_str());
        cout<<"Record is saved in file: "<<filename<<endl;
        out<<h.serialize(h);
        out.close();
    }

    int getRecord(string key,int c,OpcodeTable<K,V> h)
    {
        if(h.hasKey(key)){
          int i=h.computeHash(key),y;
          HashNode<K,V>* entry = h.record[i];
          while (entry !=  NULL)
            {
                if (entry->key == key)
                {
                    if(c==0) y=get<0>((entry->object)[0]);
                    else if(c==1) y=get<1>((entry->object)[0]);
                    else  y=get<2>((entry->object)[0]);
                    return y;
                }
             entry = entry->next;
            }
          }
        else cout<<key<<' '<<"Key does not exist."<<endl;
        return 0;
    }
};

template<typename K, typename V>
class Register : public HashTable<K,V> , public HTWrapper<K,V>
{
};

template<typename K, typename V>
class SymbolTable : public HashTable<K,V> , public HTWrapper<K,V>
{
    public:
    void printHashtable(SymbolTable<string,int> sy)
    {
        for(int i=0; i<10 ; i++)
        {
          HashNode<string,int>* ptr=sy.record[i];
          while (ptr !=  NULL)
            {
                if(ptr->object == -1) { flag=0;cout<<ptr->key<<" was never defined in the program\n";}
                else cout<<ptr->key <<" , "<<ptr->object<<endl;
                ptr=ptr->next;
            }
        }
    }
};
class Assembler
{
    public:
    bool saveToFile(string fname,string ptr)
    {
        ofstream out(fname.c_str());
        cout<<"Record is saved in file: "<<fname<<endl;
        out<<ptr;
        out.close();
        return 1;
    }
    template<typename K, typename V>
    void loadFromFile(string fname,OpcodeTable<K,V> h,SymbolTable<string,int> sy,Register<string,string> r)
    {
        string ff;
        ifstream in(fname.c_str());
        int LineNum=1, LocCtr=0,WordSize=4;
        getline(in,ff);
        while(ff[0]=='/' && ff[1]=='/')
        {
            LineNum++;
            getline(in,ff);
            
        }
        if(!validateCode(ff,h,LineNum)) flag=0;
        while(opcode!="END" && flag)
        {
            if(label!="")
            {
                if(!sy.hasKey(label))
                {
                    sy.addRecord(label,LocCtr);
                }
                else
                {
                    if(sy.getRecord(label)== -1)
                    {
                        sy.replaceRecord(label,LocCtr);
                    }
                    else {flag=0; cout<<"line "<<LineNum<<": Duplicate "<<operand[0]<<' '<<" Declaration\n";}
                }
            }
            if(opcode=="START")
            {
                stringstream st(operand[0]);
                LocCtr=0;
                st>>LocCtr;
                if(label!="")
                {
                    sy.addRecord(label,LocCtr);
                }
            }
            else if(opcode=="RESB" || opcode=="BYTE" || opcode=="RESW" || opcode=="WORD")
            {
                if(!sy.hasKey(operand[0]))
                {
                    sy.addRecord(operand[0],LocCtr);
                }
                else
                {
                    if(sy.getRecord(operand[0])== -1)
                    {
                        sy.replaceRecord(operand[0],LocCtr);
                    }
                    else {flag=0; cout<<"line "<<LineNum<<": Duplicate "<<operand[0]<<' '<<" Declaration\n";}
                }
                if(opcode=="RESB" || opcode=="BYTE") LocCtr++;
                else LocCtr+=WordSize;
            }
            else
            {
                for(int i=0;i<3 && opcode!="JUMP";i++)
                {
                    if(operand[i]!="" && !r.hasKey(operand[i]))
                    {
                        if(operand[i][0]!='0' && operand[i][0]!='1')
                        {
                            if(!sy.hasKey(operand[i]))
                            {
                                sy.addRecord(operand[i],LocCtr);
                            }
                        }
                    }
                }
                if(opcode=="JUMP")
                {
                    if(!sy.hasKey(operand[0]))
                    {
                        sy.addRecord(operand[0],-1);
                    }
                }
                LocCtr += h.getRecord(opcode,2,h);
            }
            LineNum++;
            getline(in,ff);
            while(ff[0]=='/' && ff[1]=='/')
            {
                LineNum++;
                getline(in,ff);
            }
            if(!validateCode(ff,h,LineNum))
            {
                flag=0; break;
            }
        }
        //cout<<"FOFOFO";
        sy.printHashtable(sy);
        in.close();
    }

    template<typename K, typename V>
    int validateCode(string m,OpcodeTable<K,V> opt,int count)
    {

        string d[4];
        int f=1,c=1,y=0;
        for(int j=0;j<m.size();j++){
                if(m[j]==' ') {c++;}
                else d[c-1]+=m[j];
            }
        if(d[0][d[0].size()-1]==':') y++;
        if(opt.hasKey(d[y]))
            {
                int index=opt.computeHash(d[y]);
                HashNode<K,V>* ptr=opt.record[index];
                while (ptr !=  NULL)
                {
                    if(d[y]== ptr->key)
                        {
                            int operands = get<1>((ptr->object)[0]);
                            if(operands!=c-1-y)
                            {
                                cout<<"Line "<<count<<": Invalid Number of operands for "<<d[y]<<endl; f=0;
                            }
                            break;
                        }
                    else ptr=ptr->next;
                }
            }
        else if(d[y]!="START" && d[y]!="END" && d[y]!="BYTE") {cout<<"Line "<<count<<": Unknown Operation "<<d[y]<<endl;f=0;}
        if(!fun(m)){ cout<<"line "<<count<<": Integer Overflow "<<endl; f=0;}
        return f;
    }

    template<typename K, typename V>
    void translate(string fname,string wname,OpcodeTable<K,V> h,SymbolTable<string,int> sy,Register<string,string> r)
    {
        string ff;
        ifstream in(fname.c_str());
        ofstream out(wname.c_str());
        getline(in,ff);
        while(ff[0]=='/' && ff[1]=='/') getline(in,ff);
        fun(ff);
        while(opcode!="END")
        {
            if(opcode=="START")
            {
                auto timenow = chrono::system_clock::to_time_t(chrono::system_clock::now());
                out<<"OBJ "<<operand[0]<<' '<<ctime(&timenow);
            }
            else if(opcode=="BYTE") out<<operand[1];
            else
            {
                out<<h.getRecord(opcode,0,h)<<"'";
                for(int i=0;i<3;i++)
                {
                    if(operand[i]==""){}
                    else if(r.hasKey(operand[i])) out<<r.getRecord(operand[i])<<"'";
                    else if(sy.hasKey(operand[i])) out<<sy.getRecord(operand[i])<<"'";
                    else  out<<operand[i]<<"'";
                }
            }
            out<<endl;
            getline(in,ff);
            while(ff[0]=='/' && ff[1]=='/') getline(in,ff);
            fun(ff);
        }
        cout<<"ASSEMBLER: SUCCESS! Output File: "<<wname<<endl;
        out.close();
        in.close();
    }
};
int main(){
   OpcodeTable <string,vector< tuple<int,int,int> > > opcodeTable;
   Register<string,string> reg;
   SymbolTable<string,int> symboltable;
   vector<tuple<int,int,int> > v;
   v.push_back(make_tuple(44,2,4));
   opcodeTable.addRecord("MOV",v);
   v.pop_back();
   v.push_back(make_tuple(47,2,4));
   opcodeTable.addRecord("ADD",v);
   v.pop_back();
   v.push_back(make_tuple(67,1,2));
   opcodeTable.addRecord("JUMP",v);
   v.pop_back();
   v.push_back(make_tuple(23,2,4));
   opcodeTable.addRecord("CMP",v);
   reg.addRecord("A","000");
   reg.addRecord("B","001");
   reg.addRecord("C","010");
   reg.addRecord("D","011");
   Assembler as;
   string fn="i.txt",wn="o.txt";
   as.loadFromFile(fn,opcodeTable,symboltable,reg);
   if(flag) {as.translate(fn,wn,opcodeTable,symboltable,reg); }
   else cout<<"ASSEMBLER: Found some errors in your program. Cannot proceed!"<<endl;
}

