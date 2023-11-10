using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    public class Variant
    {
        public HopperType Type { get; set; }
        public virtual Variant Clone()
        {
            throw new NotImplementedException();
        }
#if DEBUG
        public virtual void Validate()
        {
            throw new NotImplementedException();
        }
#endif
    }
    public class HopperValue : Variant
    {
        public HopperValue(uint value, HopperType vType)
        {
            Value = value;
            Type = vType;
        }
        public override Variant Clone()
        {
            HopperValue clone = new HopperValue(this.Value, this.Type);
            return clone;
        }
        public uint Value { get; set; }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
        public override bool Equals(object obj)
        {
            HopperValue val = obj as HopperValue;
            return Value.Equals(val.Value);
        }

#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Type != HopperType.tUndefined, "HopperValue validation failed");
        }
#endif

    }
    public class HopperString : Variant
    {
        public HopperString()
        {
            Type = HopperType.tString;
            Value = String.Empty;
        }

        public HopperString(string str)
        {
            Type = HopperType.tString;
            Value = str;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
        public override bool Equals(object obj)
        {
            HopperString str = obj as HopperString;
            return Value.Equals(str.Value);
        }

        public String Value { get; set; }
        public override Variant Clone()
        {
            HopperString clone = new HopperString(this.Value);
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Value != null, "HopperString validation failed");
        }
#endif
    }
    public class HopperList : Variant
    {
        public HopperList(HopperType vType)
        {
            Type = HopperType.tList;
            VType = vType;
            Value = new List<Variant>();
        }
        public HopperType VType { get; set; }
        public List<Variant> Value { get; set; }
        public override Variant Clone()
        {
            HopperList clone = new HopperList(this.VType);
            clone.Value = new List<Variant>(this.Value);
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Value != null, "HopperList validation failed");
        }
#endif
    }
    public class HopperArray : Variant
    {
        public HopperArray(HopperType vType, ushort size)
        {
            Type = HopperType.tArray;
            VType = vType;
            Value = new ushort[size];
        }
        public HopperType VType { get; set; }
        public ushort[] Value { get; set; }
        public override Variant Clone()
        {
            HopperArray clone = new HopperArray(this.VType, (ushort)this.Value.Length);
            for (uint i = 0; i < this.Value.Length; i++)
            {
                clone.Value[i] = this.Value[i]; // always value types
            }
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Value != null, "HopperArray validation failed");
        }
#endif
    }
    public class HopperStringDictionary : Variant
    {
        public HopperStringDictionary(HopperType vType)
        {
            Type = HopperType.tDictionary;
            VType = vType;
            Value = new Dictionary<string, Variant>();
        }
        public HopperType VType { get; set; }
        public Dictionary<String,Variant> Value { get; set; }
        public override Variant Clone()
        {
            HopperStringDictionary clone = new HopperStringDictionary(VType);
            foreach (KeyValuePair<string, Variant> kv in Value)
            {
                clone.Value[kv.Key] = kv.Value.Clone();
            }
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Value != null, "HopperStringDictionary validation failed");
        }
#endif
        internal bool Next(HopperPair pair, ref ushort iterator)
        {
            if (iterator < Value.Count)
            {
                KeyValuePair<String, Variant> kv = Value.ElementAt(iterator);
                pair.Key = new HopperString(kv.Key);
                pair.Value = kv.Value.Clone();
                pair.VType = kv.Value.Type;
                iterator++;
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    public class HopperUIntDictionary : Variant
    {
        public HopperUIntDictionary(HopperType vType)
        {
            Type = HopperType.tDictionary;
            VType = vType;
            Value = new Dictionary<ushort, Variant>();
        }
        public HopperType VType { get; set; }
        public Dictionary<ushort, Variant> Value { get; set; }
        public override Variant Clone()
        {
            HopperUIntDictionary clone = new HopperUIntDictionary(VType);
            foreach (KeyValuePair<ushort, Variant> kv in Value)
            {
                clone.Value[kv.Key] = kv.Value.Clone();
            }
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Value != null, "HopperUIntDictionary validation failed");
        }
#endif
        internal bool Next(HopperPair pair, ref ushort iterator)
        {
            if (iterator < Value.Count)
            {
                KeyValuePair<ushort, Variant> kv = Value.ElementAt(iterator);
                pair.Key = new HopperValue(kv.Key, HopperType.tUInt);
                pair.Value = kv.Value.Clone();
                pair.VType = kv.Value.Type;
                iterator++;
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    public class HopperPair : Variant
    {
        public HopperPair(HopperType kType, HopperType vType)
        {
            Type = HopperType.tPair;
            VType = vType;
            KType = kType;
            Value = null;
            Value = null;
        }
        public HopperType KType { get; set; }
        public HopperType VType { get; set; }
        public Variant Key { get; set; }
        public Variant Value { get; set; }
        public override Variant Clone()
        {
            throw new NotImplementedException();
        }

#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT((KType == HopperType.tString) || Runtime.Type_IsKeyType(KType), "HopperPair validation failed");
        }
#endif

    }
}
