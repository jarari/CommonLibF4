#pragma once
#include "RE/Bethesda/TESDataHandler.h"
#include <Havok.h>
#include <Windows.h>
using namespace RE;
#pragma region Utilities

namespace F4 {

	struct Unk {
		uint32_t unk00 = 0xFFFFFFFF;
		uint32_t unk04 = 0x0;
		uint32_t unk08 = 1;
	};

	namespace BSJobs {
		class JobList {
		public:
			typedef void (*Job)(void*);

			void AddJobToList(Job job, void* arg, const char* name) {
				using func_t = decltype(&F4::BSJobs::JobList::AddJobToList);
				REL::Relocation<func_t> func{ REL::ID(546314) };
				return func(this, job, arg, name);
			}

			void InsertSyncPoint(uint32_t synctype) {
				using func_t = decltype(&F4::BSJobs::JobList::InsertSyncPoint);
				REL::Relocation<func_t> func{ REL::ID(625755) };
				return func(this, synctype);
			}

			void Submit(void* local, bool b) {
				using func_t = decltype(&F4::BSJobs::JobList::Submit);
				REL::Relocation<func_t> func{ REL::ID(970923) };
				return func(this, local, b);
			}
		};
	};

	namespace bhkUtilFunctions {
		inline bhkNPCollisionObject* FindFirstCollisionObject(NiAVObject* node) {
			using func_t = decltype(&FindFirstCollisionObject);
			REL::Relocation<func_t> func{ REL::ID(507243) };
			return func(node);
		}
	}

	namespace JobListManager {
		REL::Relocation<BSJobs::JobList**> ptr_pPostPhysicsUpdateJobList{ REL::ID(1183305) };
	}

	class BSTGlobalEvent {
	public:
		virtual ~BSTGlobalEvent();

		template <typename T>
		class EventSource {
		public:
			virtual ~EventSource();

			// void ** _vtbl;                           // 00
			uint64_t unk08;         // 08
			BSTEventSource<T> src;  // 10
		};

		// void ** _vtbl;                               // 00
		uint64_t unk08;                              // 08
		uint64_t unk10;                              // 10
		BSTArray<EventSource<void*>*> eventSources;  // 18
	};

	struct Expression {
		float exp[54];
	};

	namespace FaceEmotionalIdles {
		struct InstanceData {
			uint32_t pad00;
			uint32_t handle;
			uint32_t pad08;
			float blinkTimer;
			float lidFollowEyes;
			uint64_t pad18;
			uint64_t pad20;
			uint32_t unk28;
			uint32_t pad2C;
			BSFixedString archeType;
		};
	};
	static_assert(sizeof(FaceEmotionalIdles::InstanceData) == 0x38);

	struct BSFaceGenAnimationData : public NiExtraData {
		Expression currExp;
		Expression modExp;
		Expression baseExp;
		FaceEmotionalIdles::InstanceData instanceData;
	};
	static_assert(sizeof(BSFaceGenAnimationData) == 0x2D8);

	struct EmotionalStateMachine {
		uint32_t unk00;
		uint32_t numEmotions;
		BSFixedString* emotionNames;
		Expression* emotionMorphs;
	};
	static_assert(sizeof(EmotionalStateMachine) == 0x18);

	class TES {
	public:
		NiAVObject* Pick(bhkPickData& pd) {
			using func_t = decltype(&TES::Pick);
			REL::Relocation<func_t> func{ REL::ID(1003392) };
			return func(this, pd);
		}
	};

	bool PlaySound(BGSSoundDescriptorForm* sndr, NiPoint3 pos, NiAVObject* node) {
		typedef bool* func_t(Unk, BGSSoundDescriptorForm*, NiPoint3, NiAVObject*);
		REL::Relocation<func_t> func{ REL::ID(376497) };
		Unk u;
		return func(u, sndr, pos, node);
	}

	void ShakeCamera(float mul, NiPoint3 origin, float duration, float strength) {
		using func_t = decltype(&F4::ShakeCamera);
		REL::Relocation<func_t> func{ REL::ID(758209) };
		return func(mul, origin, duration, strength);
	}

	void ApplyImageSpaceModifier(TESImageSpaceModifier* imod, float strength, NiAVObject* target) {
		using func_t = decltype(&F4::ApplyImageSpaceModifier);
		REL::Relocation<func_t> func{ REL::ID(179769) };
		return func(imod, strength, target);
	}

	TESObjectREFR* PlaceAtMe_Native(BSScript::IVirtualMachine* vm, uint32_t stackId, TESObjectREFR** target, TESForm* form, int32_t count, bool bForcePersist, bool bInitiallyDisabled, bool bDeleteWhenAble) {
		using func_t = decltype(&F4::PlaceAtMe_Native);
		REL::Relocation<func_t> func{ REL::ID(984532) };
		return func(vm, stackId, target, form, count, bForcePersist, bInitiallyDisabled, bDeleteWhenAble);
	}

	void MoveRefrToPosition(TESObjectREFR* source, uint32_t* pTargetHandle, TESObjectCELL* parentCell, TESWorldSpace* worldSpace, NiPoint3* position, NiPoint3* rotation) {
		using func_t = decltype(&F4::MoveRefrToPosition);
		REL::Relocation<func_t> func{ REL::ID(1332434) };
		return func(source, pTargetHandle, parentCell, worldSpace, position, rotation);
	}

	REL::Relocation<BSTGlobalEvent**> g_globalEvents{ REL::ID(1424022) };

	REL::Relocation<DWORD*> ptr_hkMemoryRouterTlsIndex{ REL::ID(878080) };

	REL::Relocation<uint32_t*> ptr_invalidhandle{ REL::ID(888641) };

	class ProcessLists;
	REL::Relocation<ProcessLists*> ptr_processLists{ REL::ID(474742) };

	REL::Relocation<BSGraphics::Renderer**> ptr_gRenderer{ REL::ID(1378294) };

	REL::Relocation<float*> ptr_engineTime{ REL::ID(599343) };

	REL::Relocation<float*> ptr_deltaTime{ REL::ID(1013228) };

	REL::Relocation<NiPoint3*> ptr_k1stPersonCameraLocation{ REL::ID(1304276) };

	REL::Relocation<NiPoint3*> ptr_kCurrentWorldLoc{ REL::ID(599780) };

	REL::Relocation<NiPoint3A*> ptr_PlayerAdjust{ REL::ID(988646) };

	REL::Relocation<GameUIModel**> ptr_GameUIModel{ REL::ID(17419) };

	class NiCamera;
	REL::Relocation<NiCamera*> ptr_sp1stPersonCamera{ REL::ID(380177) };

	REL::Relocation<EmotionalStateMachine*> ptr_emotionalStateMachine{ REL::ID(155349) };

	REL::Relocation<uintptr_t*> ptr_emotionalStates{ REL::ID(1380996) };

	REL::Relocation<TES*> ptr_TES{ REL::ID(1194835) };
}

namespace RE {
	enum class EShaderPropertyFlag : std::uint64_t {
		kSpecular = 1i64 << 0,
		kSkinned = 1i64 << 1,
		kTempRefraction = 1i64 << 2,
		kVertexAlpha = 1i64 << 3,
		kGrayscaleToPaletteColor = 1i64 << 4,
		kGrayscaleToPaletteAlpha = 1i64 << 5,
		kFalloff = 1i64 << 6,
		kEnvMap = 1i64 << 7,
		kRGBFalloff = 1i64 << 8,
		kCastShadows = 1i64 << 9,
		kFace = 1i64 << 10,
		kUIMaskRects = 1i64 << 11,
		kModelSpaceNormals = 1i64 << 12,
		kRefractionClamp = 1i64 << 13,
		kMultiTextureLandscape = 1i64 << 14,
		kRefraction = 1i64 << 15,
		kRefractionFalloff = 1i64 << 16,
		kEyeReflect = 1i64 << 17,
		kHairTint = 1i64 << 18,
		kScreendoorAlphaFade = 1i64 << 19,
		kLocalMapClear = 1i64 << 20,
		kFaceGenRGBTint = 1i64 << 21,
		kOwnEmit = 1i64 << 22,
		kProjectedUV = 1i64 << 23,
		kMultipleTextures = 1i64 << 24,
		kTesselate = 1i64 << 25,
		kDecal = 1i64 << 26,
		kDynamicDecal = 1i64 << 27,
		kCharacterLight = 1i64 << 28,
		kExternalEmittance = 1i64 << 29,
		kSoftEffect = 1i64 << 30,
		kZBufferTest = 1i64 << 31,
		kZBufferWrite = 1i64 << 32,
		kLODLandscape = 1i64 << 33,
		kLODObjects = 1i64 << 34,
		kNoFade = 1i64 << 35,
		kTwoSided = 1i64 << 36,
		kVertexColors = 1i64 << 37,
		kGlowMap = 1i64 << 38,
		kTransformChanged = 1i64 << 39,
		kDismembermentMeatCuff = 1i64 << 40,
		kTint = 1i64 << 41,
		kVertexLighting = 1i64 << 42,
		kUniformScale = 1i64 << 43,
		kFitSlope = 1i64 << 44,
		kBillboard = 1i64 << 45,
		kLODLandBlend = 1i64 << 46,
		kDismemberment = 1i64 << 47,
		kWireframe = 1i64 << 48,
		kWeaponBlood = 1i64 << 49,
		kHideOnLocalMap = 1i64 << 50,
		kPremultAlpha = 1i64 << 51,
		kVATSTarget = 1i64 << 52,
		kAnisotropicLighting = 1i64 << 53,
		kSkewSpecularAlpha = 1i64 << 54,
		kMenuScreen = 1i64 << 55,
		kMultiLayerParallax = 1i64 << 56,
		kAlphaTest = 1i64 << 57,
		kInvertedFadePattern = 1i64 << 58,
		kVATSTargetDrawAll = 1i64 << 59,
		kPipboyScreen = 1i64 << 60,
		kTreeAnim = 1i64 << 61,
		kEffectLighting = 1i64 << 62,
		kRefractionWritesDepth = 1i64 << 63
	};

	enum NiAVObjectFlag {
		kFlagNone = 0,
		kHidden = 1 << 0,
		kSelectiveUpdate = 1 << 1,
		kSelectiveUpdateTransforms = 1 << 2,
		kSelectiveUpdateController = 1 << 3,
		kSelectiveUpdateRigid = 1 << 4,
		kDisplayObject = 1 << 5,
		kDisableSorting = 1 << 6,
		kSelectiveUpdateTransformsOverride = 1 << 7,
		kSaveExternalGeometryData = 1 << 9,
		kNoDecals = 1 << 10,
		kAlwaysDraw = 1 << 11,
		kMeshLOD = 1 << 12,
		kFixedBound = 1 << 13,
		kTopFadeNode = 1 << 14,
		kIgnoreFade = 1 << 15,
		kNoAnimSyncX = 1 << 16,
		kNoAnimSyncY = 1 << 17,
		kNoAnimSyncZ = 1 << 18,
		kNoAnimSyncS = 1 << 19,
		kNoDismember = 1 << 20,
		kNoDismemberValidity = 1 << 21,
		kRenderUse = 1 << 22,
		kMaterialsApplied = 1 << 23,
		kHighDetail = 1 << 24,
		kForceUpdate = 1 << 25,
		kPreProcessedNode = 1 << 26
	};

	class LoadedIdleAnimData {
	public:
		BSFixedString path;			//0x00
		void* resourceHandle;		//0x08
		void* bindingWithTriggers;	//0x10
		void* clipGenerator;		//0x18
		void* hkbAnimGraph;			//0x20
	};

	class LoadingIdleAnimData {
	public:
		BSFixedString event;		//0x00
		BSFixedString path;			//0x08
		void* resourceHandle;		//0x10
		void* hkbAnimGraph;			//0x18
		void* animGraphManager;		//0x20
	};

	class TESObjectREFREx : public TESObjectREFR {
	};

	class TESObjectCELLEx : public TESObjectCELL {
	};

	class ActorEx : public Actor {
	};

	namespace BGSCharacterTint {
		class Entries {
		public:
			void Clear() {
				using func_t = decltype(&RE::BGSCharacterTint::Entries::Clear);
				REL::Relocation<func_t> func{ REL::ID(1146439) };
				return func(this);
			}
		};
	}

	namespace BGSMod {
		namespace Attachment {
			class ModEx : public Mod {
			};
		}
	}

	class BGSObjectInstanceExtraEx : public BGSObjectInstanceExtra {
	};

	class BGSProjectileEx : public BGSProjectile {
	};

	class NiPoint3Ex : public NiPoint3 {
	};

	struct TESLoadGameEvent {
	};

	namespace LoadGameEventSource {
		[[nodiscard]] BSTEventSource<TESLoadGameEvent>* GetSingleton() {
			using func_t = decltype(&RE::LoadGameEventSource::GetSingleton);
			REL::Relocation<func_t> func{ REL::ID(823570) };
			return func();
		}
	};

	BSLightingShaderProperty* FindPerPixelLighting(NiNode* node) {
		using func_t = decltype(&RE::FindPerPixelLighting);
		REL::Relocation<func_t> func{ REL::ID(1287682) };
		return func(node);
	}

	namespace GameScript {
		void PostModifyInventoryItemMod(TESObjectREFR* container, TESBoundObject* item, bool reEquip) {
			using func_t = decltype(&RE::GameScript::PostModifyInventoryItemMod);
			REL::Relocation<func_t> func{ REL::ID(1153963) };
			return func(container, item, reEquip);
		}
	}

	class CombatManager {
	public:
		static CombatManager* GetSingleton() {
			REL::Relocation<CombatManager**> singleton{ REL::ID(333664) };
			return *singleton;
		}

		float GetTargetLostPercentage(Actor* a_actor) {
			using func_t = decltype(&RE::CombatManager::GetTargetLostPercentage);
			REL::Relocation<func_t> func{ REL::ID(154107) };
			return func(this, a_actor);
		}

		float GetStealthPoints(Actor* a_actor)
		{
			using func_t = decltype(&RE::CombatManager::GetStealthPoints);
			REL::Relocation<func_t> func{ REL::ID(233530) };
			return func(this, a_actor);
		}
	};
}

char tempbuf[512] = { 0 };
char* _MESSAGE(const char* fmt, ...) {
	va_list args;

	va_start(args, fmt);
	vsnprintf(tempbuf, sizeof(tempbuf), fmt, args);
	va_end(args);
	spdlog::log(spdlog::level::warn, tempbuf);

	return tempbuf;
}

void Dump(const void* mem, unsigned int size) {
	const char* p = static_cast<const char*>(mem);
	unsigned char* up = (unsigned char*)p;
	std::stringstream stream;
	int row = 0;
	for (unsigned int i = 0; i < size; i++) {
		stream << std::setfill('0') << std::setw(2) << std::hex << (int)up[i] << " ";
		if (i % 8 == 7) {
			stream << "\t0x"
				<< std::setw(2) << std::hex << (int)up[i]
				<< std::setw(2) << (int)up[i - 1]
				<< std::setw(2) << (int)up[i - 2]
				<< std::setw(2) << (int)up[i - 3]
				<< std::setw(2) << (int)up[i - 4]
				<< std::setw(2) << (int)up[i - 5]
				<< std::setw(2) << (int)up[i - 6]
				<< std::setw(2) << (int)up[i - 7] << std::setfill('0');
			stream << "\t0x" << std::setw(2) << std::hex << row * 8 << std::setfill('0');
			_MESSAGE("%s", stream.str().c_str());
			stream.str(std::string());
			row++;
		}
	}
}

void GetShaderFlags(uint64_t flag) {
	std::string str;
	for (int i = 0; i < 64; ++i) {
		uint64_t f = 1i64 << i;
		if ((flag & f) == f) {
			if (str.length() != 0) {
				str += " ";
			}
			str += std::to_string(i);
		}
	}
	_MESSAGE("Shader Flag : %s", str.c_str());
}

template <class Ty>
Ty SafeWrite64Function(uintptr_t addr, Ty data) {
	DWORD oldProtect;
	void* _d[2];
	memcpy(_d, &data, sizeof(data));
	size_t len = sizeof(_d[0]);

	VirtualProtect((void*)addr, len, PAGE_EXECUTE_READWRITE, &oldProtect);
	Ty olddata;
	memset(&olddata, 0, sizeof(Ty));
	memcpy(&olddata, (void*)addr, len);
	memcpy((void*)addr, &_d[0], len);
	VirtualProtect((void*)addr, len, oldProtect, &oldProtect);
	return olddata;
}

ActorValueInfo* GetAVIFByEditorID(std::string editorID) {
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	BSTArray<ActorValueInfo*> avifs = dh->GetFormArray<ActorValueInfo>();
	for (auto it = avifs.begin(); it != avifs.end(); ++it) {
		if (strcmp((*it)->formEditorID.c_str(), editorID.c_str()) == 0) {
			return (*it);
		}
	}
	return nullptr;
}

BGSExplosion* GetExplosionByFullName(std::string explosionname) {
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	BSTArray<BGSExplosion*> explosions = dh->GetFormArray<BGSExplosion>();
	for (auto it = explosions.begin(); it != explosions.end(); ++it) {
		if (strcmp((*it)->GetFullName(), explosionname.c_str()) == 0) {
			return (*it);
		}
	}
	return nullptr;
}

SpellItem* GetSpellByFullName(std::string spellname) {
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	BSTArray<SpellItem*> spells = dh->GetFormArray<SpellItem>();
	for (auto it = spells.begin(); it != spells.end(); ++it) {
		if (strcmp((*it)->GetFullName(), spellname.c_str()) == 0) {
			return (*it);
		}
	}
	return nullptr;
}

BGSMaterialType* GetMaterialTypeByName(std::string materialname) {
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	BSTArray<BGSMaterialType*> materials = dh->GetFormArray<BGSMaterialType>();
	BSFixedString matname = BSFixedString(materialname);
	for (auto it = materials.begin(); it != materials.end(); ++it) {
		if ((*it)->materialName == matname) {
			return (*it);
		}
	}
	return nullptr;
}

EffectSetting* GetMagicEffectByFullName(std::string effectname) {
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	BSTArray<EffectSetting*> mgefs = dh->GetFormArray<EffectSetting>();
	for (auto it = mgefs.begin(); it != mgefs.end(); ++it) {
		if (strcmp((*it)->GetFullName(), effectname.c_str()) == 0) {
			return (*it);
		}
	}
	return nullptr;
}

TESForm* GetFormFromMod(std::string modname, uint32_t formid) {
	if (!modname.length() || !formid)
		return nullptr;
	TESDataHandler* dh = TESDataHandler::GetSingleton();
	return dh->LookupForm(formid, modname);
}

const char* GetObjectClassNameImpl(const char* result, void* objBase) {
	using namespace RTTI;
	void** obj = (void**)objBase;
	CompleteObjectLocator** vtbl = (CompleteObjectLocator**)obj[0];
	CompleteObjectLocator* rtti = vtbl[-1];
	RVA<TypeDescriptor> typeDesc = rtti->typeDescriptor;

	// starts with ,?
	const char* _name = typeDesc->mangled_name();
	if ((_name[0] == '.') && (_name[1] == '?')) {
		// is at most 100 chars long
		for (uint32_t i = 0; i < 100; i++) {
			if (_name[i] == 0) {
				// remove the .?AV
				return _name + 4;
				break;
			}
		}
	}
	return result;
}

const char* GetObjectClassName(void* objBase) {
	const char* result = "<no rtti>";
	__try {
		result = GetObjectClassNameImpl(result, objBase);
	}
	__except (EXCEPTION_EXECUTE_HANDLER) {
		// return the default
	}

	return result;
}

NiNode* CreateBone(const char* name) {
	NiNode* newbone = new NiNode(0);
	newbone->name = name;
	return newbone;
}

bool CheckPA(Actor* a) {
	if (!a || !a->extraList) {
		return false;
	}
	return a->extraList->HasType(EXTRA_DATA_TYPE::kPowerArmor);
	;
}

bool GetIsSighted(Actor* a) {
	return (a->gunState == GUN_STATE::kSighted || a->gunState == GUN_STATE::kFireSighted);
}

float GetActorScale(Actor* a) {
	typedef float (*_GetPlayerScale)(Actor*);
	REL::Relocation<_GetPlayerScale> func{ REL::ID(911188) };
	return func(a);
}

void ReloadWeaponGraph(BSExtraData* animGraphPreload, Actor& a_actor) {
	typedef void (*_ReloadWeaponGraph)(BSExtraData*, Actor&);
	REL::Relocation<_ReloadWeaponGraph> func{ REL::ID(393711) };
	return func(animGraphPreload, a_actor);
}

bool Visit(NiAVObject* obj, const std::function<bool(NiAVObject*)>& functor) {
	if (functor(obj))
		return true;

	NiPointer<NiNode> node(obj->IsNode());
	if (node) {
		for (auto it = node->children.begin(); it != node->children.end(); ++it) {
			NiPointer<NiAVObject> object(*it);
			if (object) {
				if (Visit(object.get(), functor))
					return true;
			}
		}
	}

	return false;
}

class RopeDelegate : public F4SE::ITaskDelegate {
public:
	TESObjectREFREx* rope;
	NiPoint3 targetPos;
	TESObjectCELL* targetCell;
	TESWorldSpace* targetWorld;
	RopeDelegate(TESObjectREFR* ref, NiPoint3 pos, TESObjectCELL* cell, TESWorldSpace* world) {
		rope = (TESObjectREFREx*)ref;
		targetPos = pos;
		targetCell = cell;
		targetWorld = world;
	}
	virtual void Run() override {
		uint32_t nullHandle = *F4::ptr_invalidhandle;
		NiPoint3 rot;
		F4::MoveRefrToPosition(rope, &nullHandle, targetCell, targetWorld, &targetPos, &rot);
	}
};

void MoveBendableSpline(TESObjectREFR* rope, NiPoint3 start, NiPoint3 end, TESObjectCELL* cell, TESWorldSpace* world, bool useQueue = true) {
	TaskQueueInterface* queue = TaskQueueInterface::GetSingleton();

	uint32_t nullHandle = *F4::ptr_invalidhandle;
	NiPoint3 rot;
	NiPoint3 ropePos = (start + end) / 2.f;
	F4::MoveRefrToPosition(rope, &nullHandle, cell, world, &ropePos, &rot);

	float thickness = 1.0f;
	float slack = 0.f;
	ExtraBendableSplineParams* splineParams = (ExtraBendableSplineParams*)rope->extraList->GetByType(EXTRA_DATA_TYPE::kBendableSplineParams);
	if (splineParams) {
		thickness = splineParams->thickness;
	}
	NiPoint3 offset = (start - end) / 2.f;
	rope->extraList->SetBendableSplineInfo(&thickness, &slack, &offset);

	if (useQueue) {
		queue->QueueRebuildBendableSpline(*rope, true, nullptr);
	}
	else {
		((TESObjectREFREx*)rope)->RebuildBendableSpline(false, nullptr);
	}
	//F4SE::GetTaskInterface()->AddTask(new RopeDelegate(rope, ropePos, cell, world));
}

std::string SplitString(const std::string str, const std::string delimiter, std::string& remainder) {
	std::string ret;
	size_t i = str.find(delimiter);
	if (i == std::string::npos) {
		ret = str;
		remainder = "";
		return ret;
	}

	ret = str.substr(0, i);
	remainder = str.substr(i + 1);
	return ret;
}

void SetupFilter(bhkPickData& pick, Actor* a, BGSProjectile* projForm, bool excludeActor) {
	uint32_t index = 6;
	uint64_t flag = 0x1C15160;
	if (projForm && projForm->data.collisionLayer) {
		index = projForm->data.collisionLayer->collisionIdx;
		if (!((BGSProjectileEx*)projForm)->CollidesWithSmallTransparentLayer())
			flag = 0x15C15160;
	}
	uint64_t filter = *(uint64_t*)((*REL::Relocation<uint64_t*>{ REL::ID(469495) }) + 0x1A0 + 0x8 * index) | 0x40000000;
	*(uint64_t*)((uintptr_t)&pick + 0xC8) = filter & ~flag;
	uint32_t collisionGroup = 6;
	if (excludeActor && a->loadedData && (*(BYTE*)((uintptr_t)a->loadedData + 0x20) & 0x1)) {
		collisionGroup = ((ActorEx*)a)->GetCurrentCollisionGroup();
	}
	*(uint32_t*)((uintptr_t)&pick + 0x0C) = ((collisionGroup << 16) | 0x29);
}

NiAVObject* GetPickDataCELL(const NiPoint3& start, const NiPoint3& end, Actor* a, bhkPickData& pick, bool excludeActor = true) {
	if (!a->parentCell)
		return nullptr;
	bhkWorld* world = a->parentCell->GetbhkWorld();
	if (!world)
		return nullptr;
	hknpBSWorld* hkWorld = *(hknpBSWorld**)((uintptr_t)world + 0x60);
	if (!hkWorld)
		return nullptr;

	pick.SetStartEnd(start, end);
	SetupFilter(pick, a, nullptr, excludeActor);
	return ((TESObjectCELLEx*)a->parentCell)->Pick(pick);
}

NiAVObject* GetPickDataTES(const NiPoint3& start, const NiPoint3& end, Actor* a, bhkPickData& pick, bool excludeActor = true) {
	pick.SetStartEnd(start, end);
	SetupFilter(pick, a, nullptr, excludeActor);
	return F4::ptr_TES->Pick(pick);
}

bool GetPickData(const NiPoint3& start, const NiPoint3& end, Actor* a, BGSProjectile* projForm, bhkPickData& pick, bool excludeActor = true) {
	if (!a->parentCell)
		return false;
	bhkWorld* world = a->parentCell->GetbhkWorld();
	if (!world)
		return false;
	hknpBSWorld* hkWorld = *(hknpBSWorld**)((uintptr_t)world + 0x60);
	if (!hkWorld)
		return false;

	pick.SetStartEnd(start, end);
	hknpClosestHitCollector collector = hknpClosestHitCollector();
	*(uintptr_t*)((uintptr_t)&pick + 0xD0) = (uintptr_t)&collector;
	*(uint32_t*)((uintptr_t)&pick + 0xD8) = 3;
	SetupFilter(pick, a, projForm, excludeActor);
	return CombatUtilities::CalculateProjectileLOS(a, projForm, pick);
}

bool GetPickDataAll(const NiPoint3& start, const NiPoint3& end, Actor* a, BGSProjectile* projForm, bhkPickData& pick, bool excludeActor = true) {
	if (!a->parentCell)
		return false;
	bhkWorld* world = a->parentCell->GetbhkWorld();
	if (!world)
		return false;
	hknpBSWorld* hkWorld = *(hknpBSWorld**)((uintptr_t)world + 0x60);
	if (!hkWorld)
		return false;

	pick.SetStartEnd(start, end);
	hknpAllHitsCollector collector = hknpAllHitsCollector();
	*(uintptr_t*)((uintptr_t)&pick + 0xD0) = (uintptr_t)&collector;
	*(uint32_t*)((uintptr_t)&pick + 0xD8) = 0;
	SetupFilter(pick, a, projForm, excludeActor);
	return CombatUtilities::CalculateProjectileLOS(a, projForm, pick);
}

NiPoint3 GetProjectileNode(Actor* a, PlayerCamera* pcam) {
	if (a && a->Get3D()) {
		NiNode* endPoint = (NiNode*)a->Get3D()->GetObjectByName("ProjectileNode");
		if (!endPoint) {
			endPoint = (NiNode*)a->Get3D()->GetObjectByName("Weapon");
		}
		NiPoint3 newPos = endPoint->world.translate;
		if (a->Get3D(true) == a->Get3D()) {
			NiNode* camera = (NiNode*)a->Get3D()->GetObjectByName("Camera");
			newPos = newPos + pcam->cameraRoot->world.translate - camera->world.translate;
		}
		return newPos;
	}
	return NiPoint3();
}

#pragma endregion
