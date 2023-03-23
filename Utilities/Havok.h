#pragma once
#include <stdint.h>
#include "RE/RTTI_IDs.h"
#include "RE/VTABLE_IDs.h"
#include "RE/Havok/hkVector4.h"
#include "RE/Havok/hknpShape.h"
#include "RE/Havok/hknpMaterialId.h"
#include "RE/NetImmerse/NiNode.h"
#include "RE/NetImmerse/NiMatrix3.h"

namespace RE {
	struct hknpClosestPointsQuery;
	class hknpCollisionQueryCollector;
	struct hknpCollisionFilter;
	struct hknpShapeCastQuery;
	struct hknpTriangleShape;
	struct hknpPhysicsSystemData;
	struct hknpWorld;
	class bhkCharProxyController;
	class hknpShapeTagCodec;

	REL::Relocation<bool*> ptr_bWorldUpdatedLastFrame{ REL::ID(286044) };

	REL::Relocation<bool*> ptr_bPickCountOverride{ REL::ID(511357) };

	REL::Relocation<bool*> ptr_bVatsReplay{ REL::ID(736908) };

	REL::Relocation<float*> ptr_fMaxPickTime{ REL::ID(195013) };

	REL::Relocation<float*> ptr_fMaxPickTimeVATS{ REL::ID(1095550) };

	REL::Relocation<float*> ptr_fCurrentPickTime{ REL::ID(702049) };

	REL::Relocation<float*> ptr_fBS2HkScale{ REL::ID(1126486) };

	REL::Relocation<hkSimdFloat32*> ptr_earlyOutThreshold{ REL::ID(781602) };

	REL::Relocation<hkMemoryAllocator*> ptr_containerHeapAllocator{ REL::ID(409811) };

	REL::Relocation<hknpShapeTagCodec*> ptr_shapeTagCodec{ REL::ID(322467) };

	REL::Relocation<hknpCollisionFilter*> ptr_collisionFilter{ REL::ID(246130) };

	REL::Relocation<bhkWorld***> ptr_bhkWorldM{ REL::ID(128691) };

	struct ImpulseData {
		NiPoint3 dir;
		float mag;
		hknpWorld* world;
		ImpulseData(NiPoint3 _d, float _m, hknpWorld* _w) {
			dir = _d;
			mag = _m;
			world = _w;
		}
	};

	void ApplyHavokImpulse(NiNode* node, ImpulseData data) {
		using func_t = decltype(&RE::ApplyHavokImpulse);
		REL::Relocation<func_t> func{ REL::ID(251131) };
		return func(node, data);
	}

	class hknpBSWorld {
	public:
		void MarkForRead() {
			using func_t = decltype(&RE::hknpBSWorld::MarkForRead);
			REL::Relocation<func_t> func{ REL::ID(1190448) };
			return func(this);
		}

		void UnmarkForRead() {
			using func_t = decltype(&RE::hknpBSWorld::UnmarkForRead);
			REL::Relocation<func_t> func{ REL::ID(485648) };
			return func(this);
		}

		void MarkForWrite() {
			using func_t = decltype(&RE::hknpBSWorld::MarkForWrite);
			REL::Relocation<func_t> func{ REL::ID(1017544) };
			return func(this);
		}

		void UnmarkForWrite() {
			using func_t = decltype(&RE::hknpBSWorld::UnmarkForWrite);
			REL::Relocation<func_t> func{ REL::ID(312883) };
			return func(this);
		}
	};

	struct hknpThreadSafetyCheck {
	public:
		void acquireReadAccess() {
			using func_t = decltype(&RE::hknpThreadSafetyCheck::acquireReadAccess);
			REL::Relocation<func_t> func{ REL::ID(1316439) };
			return func(this);
		}

		void releaseReadAccess() {
			using func_t = decltype(&RE::hknpThreadSafetyCheck::releaseReadAccess);
			REL::Relocation<func_t> func{ REL::ID(598242) };
			return func(this);
		}
	};

	struct CollisionResult {
		hkVector4f pos;
		hkVector4f normal;
		float unk20[3];
		uint32_t materialID; //??
		float unk30[4];
	};

	class hknpPhysicsSystem :
		public hkReferencedObject {
	public:
		hknpPhysicsSystemData* m_data;
		hknpWorld* m_world;
	};

	class bhkPhysicsSystem :
		public NiObject {
	public:
		hknpPhysicsSystemData* physicsSystemData;
		hknpPhysicsSystem* physicsSystem;
	};

	struct BoundingBoxVolume {
		hkVector4f vertex[8];
	};

	struct hknpDynamicCompoundShapeData : hkReferencedObject {
		BoundingBoxVolume* bbv;
	};

	struct CompoundShapeData {
		hkVector4f rotation[3];
		hkVector4f translate;
		hkVector4f scale;
		hknpShape* shape;
		uint8_t pad[0x80 - 0x58];
	};
	static_assert(sizeof(CompoundShapeData) == 0x80);

	struct MultiCompoundShape {
		CompoundShapeData data[3];
	};

	struct unkParam {
	public:
		uint64_t unk = 0;
		float out = 0;
	};

	struct CastResult {
	public:
		struct Data {
			hkVector4f normal;
			hkVector4f unk;
			hkVector4f localHit;
		};
		Data* data;
	};

	class __declspec(novtable) hkaRaycastInterface {
	public:
		static constexpr auto RTTI{ RTTI::hkaRaycastInterface };
		static constexpr auto VTABLE{ VTABLE::hkaRaycastInterface };

		virtual ~hkaRaycastInterface() = default;

		virtual hkVector4f castRay(unkParam, const hkVector4f, const hkVector4f, uint32_t, float&, hkVector4f&) = 0;

		virtual hkVector4f castRay(unkParam, const hkVector4f, const hkVector4f, float&, hkVector4f&) = 0;
	};

	class hkbSpatialQueryInterface : public hkaRaycastInterface {
	public:
		static constexpr auto RTTI{ RTTI::hkbSpatialQueryInterface };
		static constexpr auto VTABLE{ VTABLE::hkbSpatialQueryInterface };
	};

	class CachedRaycastData : public hkbSpatialQueryInterface {
	public:
		static constexpr auto RTTI{ RTTI::CachedRaycastData };
		static constexpr auto VTABLE{ VTABLE::CachedRaycastData };

		uintptr_t _vtable;
		uint32_t unk08;
		uint32_t unk0C;
		bhkWorld* world;
		uint32_t unk18;
		uint16_t unk1C;
		uint8_t unk1E;
		bool unk1F;
		F4_HEAP_REDEFINE_NEW(CachedRaycastData);
	};

	class FakeCachedRaycastData {
	public:
		static constexpr auto RTTI{ RTTI::CachedRaycastData };
		static constexpr auto VTABLE{ VTABLE::CachedRaycastData };

		FakeCachedRaycastData() {
			_vtable = REL::Relocation<uint64_t>{ FakeCachedRaycastData::VTABLE[0] }.address();
			unk08 = 0x0;
			unk18 = 0x591;
		}
		uintptr_t _vtable;
		uint32_t unk08;
		uint32_t unk0C;
		bhkWorld* world;
		uint32_t unk18;
		uint16_t unk1C;
		uint8_t unk1E;
		bool unk1F;
		uint64_t unk20;
	};

	struct hkAabb16 {
	public:
		uint16_t m_min[3];
		uint16_t m_key;
		uint16_t m_max[3];
		uint16_t m_key1;
	};

	class hknpBody {
		// +version(1)
	public:

		/// Body Flags.
		/// These flags use the lower 20 bits of a 32 bit field, and can be OR'ed with hknpMaterial::Flags to produce
		/// a 32 bit flags value. A modifier is executed if at least one of these 32 flags is registered with any
		/// modifier in hknpModifierManager.
		enum FlagsEnum {
			//
			// Internal flags. These are not user controllable.
			// Valid bodies must have either the IS_STATIC or IS_DYNAMIC flag set.
			//

			IS_STATIC = 1 << 0,		///< Set for bodies using the static motion.
			IS_DYNAMIC = 1 << 1,		///< Set for bodies using a dynamic motion.
			IS_KEYFRAMED = 1 << 2,		///< Set for bodies using a dynamic motion with infinite mass.
			IS_ACTIVE = 1 << 3,		///< Set for bodies using a dynamic motion which is currently active.

			//
			// Flags to enable event raising modifiers.
			//

			/// Raise a hknpTriggerVolumeEvent whenever a shape using a trigger volume material is entered or exited.
			/// See hknpMaterial::m_triggerVolumeType and hknpMaterial::m_triggerVolumeTolerance for related settings.
			RAISE_TRIGGER_VOLUME_EVENTS = 1 << 4,

			/// Raise a hknpManifoldStatusEvent whenever a contact manifold using this body is created or destroyed
			/// during collision detection.
			RAISE_MANIFOLD_STATUS_EVENTS = 1 << 5,

			/// Raise a hknpManifoldProcessedEvent whenever a contact manifold using this body is processed
			/// during collision detection, including the step when a manifold is created.
			RAISE_MANIFOLD_PROCESSED_EVENTS = 1 << 6,

			/// Raise a hknpContactImpulseEvent whenever a contact Jacobian using this body starts applying non-zero
			/// impulses during solving. Events can be raised for subsequent solver steps by calling
			/// hknpContactImpulseEvent::set[Continued|Finished]EventsEnabled() from your event handler.
			RAISE_CONTACT_IMPULSE_EVENTS = 1 << 7,

			//
			// Flags to enable special simulation behaviors.
			//

			/// Disable all collision cache creation for this body, independent of how the collision filter is set up.
			/// This can be used if a body is only ever intended to be queried, for example.
			DONT_COLLIDE = 1 << 8,

			/// Disable all contact solving for this body.
			/// This is typically set in the collision cache flags by the trigger volume modifier.
			DONT_BUILD_CONTACT_JACOBIANS = 1 << 9,

			//
			// Flags that are cleared after every simulation step. These are used to either:
			//  - trigger specific collision detection functions, or
			//  - trigger single step modifiers
			//

			/// Force collision caches to be rebuilt during the next collision detection step.
			TEMP_REBUILD_COLLISION_CACHES = 1 << 10,

			/// Allows for optimizations of exploding debris scenarios, see
			/// hknpMaterial::m_disablingCollisionsBetweenCvxCvxDynamicObjectsDistance.
			TEMP_DROP_NEW_CVX_CVX_COLLISIONS = 1 << 11,

			TEMP_USER_FLAG_0 = 1 << 12,
			TEMP_USER_FLAG_1 = 1 << 13,

			/// Do not add the body to the world.
			IS_NON_RUNTIME = 1 << 14,	// This flag is temporary and will be removed in the future.

			/// Destruction specific. Indicates the body can break into pieces
			IS_BREAKABLE = 1 << 15,

			//
			// User flags. Can be used to enable user modifiers.
			//

			USER_FLAG_0 = 1 << 16,
			USER_FLAG_1 = 1 << 17,
			USER_FLAG_2 = 1 << 18,
			USER_FLAG_3 = 1 << 19,

			//
			// Masks
			//

			FLAGS_MASK = 0x000fffff,	///< Mask of all allowed flags
			INTERNAL_FLAGS_MASK = 0xf << 0,		///< Mask of all "internal" flags.
			EVENT_FLAGS_MASK = 0xf << 4,		///< Mask of all "event" flags.
			TEMP_FLAGS_MASK = 0xf << 10		///< Mask of all "temp" flags.
		};

		typedef hkFlags<FlagsEnum, uint32_t> Flags;

		enum SpuFlagsEnum {
			FORCE_NARROW_PHASE_PPU = 1 << 0,

			// Not supported yet
			// FORCE_SOLVER_PPU		= 1 << 1
		};

		typedef hkFlags<SpuFlagsEnum, uint8_t> SpuFlags;

	public:

		/// Body position and orientation in world space in the XYZ components, and the following in the W components:
		///  - W[0,1,2] : mass center in local space
		///  - W[3]     : look ahead distance
		hkTransform m_transform;

		//friend struct hknpInternalMotionUtilFunctions2;



		/// The expanded AABB of the body in integer space.
		/// Use hknpWorld::m_intSpaceUtil to convert to a floating point hkAabb.
		hkAabb16 m_aabb;



		/// The collision shape of the body.
		/// Note that this is not reference counted.
		const hknpShape* m_shape;

		/// Body flags. See FlagsEnum.
		Flags m_flags;

		/// Filter information for hknpCollisionFilter.
		uint32_t m_collisionFilterInfo;



		/// This body's identifier. This can be used in the hknpWorld::setBodyXxx() methods.
		//hknpBodyId m_id; //+overridetype(hkUint32)

		/// Identifier of the next body which shares the same motion.
		/// All bodies attached to a motion form a ring. To gather all bodies sharing this motion, iterate through them
		/// until you reach this body's ID again.
		//hknpBodyId m_nextAttachedBodyId; //+overridetype(hkUint32)

		/// The motion identifier. Use hknpWorld::getMotion() to retrieve the motion.
		/// Note that static bodies always have a motion ID of zero.
		//hknpMotionId m_motionId; //+overridetype(hkUint32)

		/// The material identifier. Use hknpWorld::getMaterial() to retrieve the material.
		/// Note that composite shape children can override their material by using a Shape Tag Codec.
		//hknpMaterialId m_materialId; //+overridetype(hkUint16)

		/// The quality identifier. Use hknpWorld::getQuality() to retrieve the quality.
		//hknpBodyQualityId m_qualityId; //+overridetype(hkUint8)

		/// Byte size of the shape divided by 16. Used on SPU for DMA transfers.
		//hkUint8 m_shapeSizeDiv16;

	};


	template <typename T>
	class hkRelArray {
	protected:
		uint16_t m_size;
		uint16_t m_offset;
	};

	class hknpConvexShape : public hknpShape {
	public:
		hkRelArray<hkcdVertex> m_vertices;
	};

	class hknpConvexPolytopeShape : public hknpConvexShape {
		//+version(1)

	public:

		/// A face
		struct Face {
			uint8_t		m_minHalfAngle;	///< Minimum half angle between this and neighboring faces. (255 = 90deg).
			uint8_t		m_numIndices;	///< Number of indices.
			uint16_t	m_firstIndex;	///< First vertex index (hknpConvexPolytopeShape::m_indices + m_firstIndex)
		};

		hkRelArray<hkVector4f>	m_planes;	///< Offset to planes stream.
		hkRelArray<Face>		m_faces;	///< Offset to faces stream.
		hkRelArray<uint8_t>	m_indices;	///< Offset to indices stream.
	};

	struct hknpTriangleShape : public hknpConvexPolytopeShape {
	public:

	};

	class hknpShapeTagCodec :
		public hkReferencedObject {
	public:
		static constexpr auto RTTI{ RTTI::hknpShapeTagCodec };
		static constexpr auto VTABLE{ VTABLE::hknpShapeTagCodec };
		/// Codec types
		enum CodecType {
			NULL_CODEC,
			MATERIAL_PALETTE_CODEC,
			UFM_CODEC,
			USER_CODEC
		};
		hknpShapeTagCodec(CodecType type) : m_type(type) {}

		/// Codec type
		hkEnum<CodecType, uint8_t> m_type;
	};


	typedef uint32_t hknpShapeKey;
	struct hknpShapeKeyPath {
	protected:

		/// The current key path, concatenated from all sub keys that have been added using appendSubKey().
		// Note: Do *NOT* pad this value on SPU as we need to allocate a large block of shape key paths in the
		//       hknpCompositeCompositeCollisionDetector. This additional required stack space would outweigh the
		//       small code size increase caused by not padding this value!
		hknpShapeKey m_key = 0xFFFFFFFF;

		/// The key path's current size in bits.
		// Note: Do *NOT* pad this value on SPU as we need to allocate a large block of shape key paths in the
		//       hknpCompositeCompositeCollisionDetector. This additional required stack space would outweigh the
		//       small code size increase caused by not padding this value!
		int m_size;
	};

	struct hkcdRayQueryFlags {
		enum Enum {
			/// No flags. Implies default behavior:
			///  - Triangles are treated as two sided.
			///  - No inside hits are reported.
			NO_FLAGS = 0,

			/// In triangle based shapes (e.g: mesh, height field), rays will not hit triangle back faces.
			/// Back faces are considered to be those with clockwise vertex winding.
			DISABLE_BACK_FACING_TRIANGLE_HITS = 1 << 0,

			/// In triangle based shapes (e.g: mesh, height field), rays will not hit triangle front faces.
			/// Front faces are considered to be those with counter-clockwise vertex winding.
			DISABLE_FRONT_FACING_TRIANGLE_HITS = 1 << 1,

			/// In non-triangle based shapes, rays will hit the insides of the shape.
			/// The normal returned for inside hits will be outward facing.
			/// Note: For shapes created using hknpConvexShape::createFromVertices(), this flag only works when
			/// hknpConvexShape::BuildConfig.m_buildFaces is set to true.
			ENABLE_INSIDE_HITS = 1 << 2
		};
	};

	struct hknpQueryFilterData {
	public:

		hknpMaterialId			m_materialId;			///< The material id associated with the shape. Allowed to be 'invalid'.
		uint32_t	m_collisionFilterInfo = 0x29EE23;	///< The collision filter info associated with the shape.
		uint64_t	m_userData = 0x5C;				///< The user data associated with the shape.
	};

	struct hkcdRay {
	public:
		hkcdRay() {};
		float getFraction() { return m_direction.w; };
		/// An optional filter associated with this query.
		hkPadSpu< hknpCollisionFilter* > m_filter;

		/// Additional filter data associated with the ray.
		hknpQueryFilterData m_filterData;

		/// Flags for customizing the ray behavior.
		uint32_t m_flags;

		uint64_t unk16 = 0x1F72F1D9D000000;

		uint16_t unk1E;

		/// Origin of the ray.
		hkVector4f	m_origin;

		/// Direction of the ray. Effectively the (endPoint - startPoint) vector (and length as |direction|).
		hkVector4f	m_direction;

		/// Precomputed direction reciprocal with +/-Inf == 0.
		/// In addition, the mask of the following comparison is stored in the W component via setInt24W: m_direction >= [0,0,0].
		hkVector4f	m_invDirection;
	};

	struct hknpRayCastQuery : public hkcdRay {
	public:

		/// Initializes the ray to be cast from \a start position (world space) to \a end position (world space)
		/// and sets the remaining members to their default values.
		hknpRayCastQuery(hkVector4f start, hkVector4f end) {
			m_origin = start;
			m_direction = (end - start).GetNormalized();
			m_direction.w = 1;
		}

		/// Initializes the ray to be cast from \a start position (world space) along the \a direction (normalized)
		/// up to a maximum \a length and sets the remaining members to their default values.
		hknpRayCastQuery(hkVector4f start, hkVector4f direction, float length) {
			m_origin = start;
			m_direction = direction;
			m_direction.w = length;
		}


	};

	struct hknpShapeQueryInfo {
	public:

		hkPadSpu< const hknpBody* >			m_body;				///< The shape's governing body. Can be set to HK_NULL.
		hkPadSpu< const hknpShape* >		m_rootShape;		///< The shape's topmost ancestor. Identical to the shape in m_body (if available).
		hkPadSpu< const hknpShape* >		m_parentShape;		///< The shape's parent shape. HK_NULL for top-level shapes.
		hknpShapeKeyPath					m_shapeKeyPath;		///< The shape's key path.
		hkPadSpu< const hkTransform* >		m_shapeToWorld;		///< The shape's world transform. This value is MANDATORY!
		hkPadSpu< const hknpShapeKeyMask* >	m_shapeKeyMask;		///< The shape's (optional) shape key mask.

		hkPadSpu< hkReal >					m_shapeConvexRadius;///< This value will be automatically set by the engine. The convex radius for the shape. This value will be ignored if all shapes involved in a spatial query are unscaled.
		hkPadSpu< uint32_t >				m_shapeIsScaled;	///< This value will be automatically set by the engine. Non-zero if shape is scaled.
		hkVector4f							m_shapeScale;		///< This value will be automatically set by the engine. The scaling vector for the shape (if scaling is enabled).
		hkVector4f							m_shapeScaleOffset;	///< This value will be automatically set by the engine. The offset for the shape (if scaling is enabled).
	};


	class hknpCollisionQueryDispatcherBase {
	public:

		/// Base shape types, used in the dispatch tables.
		enum BaseType {
			// Convex types
			CONVEX,

			// Composite types
			COMPRESSED_MESH,
			EXTERN_MESH,
			STATIC_COMPOUND,
			DYNAMIC_COMPOUND,

			// Height field types
			HEIGHT_FIELD,

			// Wrapper types
			MASKED_COMPOSITE,
			SCALED_CONVEX,

			// User defined types
			USER,

			NUM_BASE_TYPES
		};

		/// A bitfield where each bit represent one of BaseType. Used to configure the query dispatcher initialization.
		typedef uint32_t ShapeMask;
		static const ShapeMask defaultShapeMask = 0 \
			| 1 << hknpCollisionQueryDispatcherBase::CONVEX \
			| 1 << hknpCollisionQueryDispatcherBase::COMPRESSED_MESH \
			| 1 << hknpCollisionQueryDispatcherBase::STATIC_COMPOUND \
			| 1 << hknpCollisionQueryDispatcherBase::DYNAMIC_COMPOUND \
			/* 1 << hknpCollisionQueryDispatcherBase::HEIGHT_FIELD*/ \
			| 1 << hknpCollisionQueryDispatcherBase::MASKED_COMPOSITE \
			| 1 << hknpCollisionQueryDispatcherBase::SCALED_CONVEX \
			/*| 1 << hknpCollisionQueryDispatcherBase::USER*/;

		/// A dispatch table for a given function type.
		template<typename FUNCTION_TYPE>
		struct DispatchTable {
			FUNCTION_TYPE m_dispatchTable[NUM_BASE_TYPES][NUM_BASE_TYPES];
		};

		/// A function to cast one shape against another.
		typedef void (hknpCollisionQueryDispatcherBase::* ShapeCastFunc)(
			hknpCollisionQueryContext* queryContext,
			const hknpShapeCastQuery& query, const hknpShapeQueryInfo& queryShapeInfo,
			const hknpShape* targetShape, const hknpQueryFilterData& targetShapeFilterData, const hknpShapeQueryInfo& targetShapeInfo,
			const hkTransform& queryToTarget, bool queryAndTargetSwapped,
			hknpCollisionQueryCollector* collector);

		/// A function to get the closest points between a pair of shapes.
		typedef void (hknpCollisionQueryDispatcherBase::* ClosestPointsFunc)(
			hknpCollisionQueryContext* queryContext,
			const hknpClosestPointsQuery& query, const hknpShapeQueryInfo& queryShapeInfo,
			const hknpShape* targetShape, const hknpQueryFilterData& targetShapeFilterData, const hknpShapeQueryInfo& targetShapeInfo,
			const hkTransform& queryToTarget, bool queryAndTargetSwapped,
			hknpCollisionQueryCollector* collector);
		/// A map of concrete shape types to base types used for dispatching.
		BaseType m_baseTypeMap[(uint64_t)hknpShapeType::Enum::kTotal];

	public:

		/// A dispatch table for closest point queries.
		DispatchTable<ClosestPointsFunc> m_closestPointsDispatchTable;

		/// A dispatch table for shape cast queries.
		DispatchTable<ShapeCastFunc> m_shapeCastDispatchTable;
	};

	struct hknpCollisionQueryContext {
	public:
		hknpCollisionQueryContext(hknpTriangleShape* queryTriangle, hknpTriangleShape* targetTriangle) {
			m_queryTriangle = queryTriangle;
			m_targetTriangle = targetTriangle;
			m_dispatcher = nullptr;
			m_shapeTagCodec = nullptr;
			m_queryTriangle = nullptr;
			m_targetTriangle = nullptr;
			if (queryTriangle && targetTriangle)
				m_externallyAllocatedTriangles = 1;

		}
		/// The collision query dispatcher to use for pairwise shape queries.
		hknpCollisionQueryDispatcherBase* m_dispatcher;

		/// The codec to use for decoding shape tags during a collision query.
		/// Some values in hknpCollisionResult will only be valid if a shape tag codec has been provided.
		/// This value *has* to be set if filtering is enabled for a spatial query.
		hknpShapeTagCodec* m_shapeTagCodec;

		/// A pre-allocated but uninitialized triangle shape.
		/// This can be used temporarily by the collision (query) pipeline.
		hknpTriangleShape* m_queryTriangle;

		/// A pre-allocated but uninitialized triangle shape.
		/// This can be used temporarily by the collision (query) pipeline.
		hknpTriangleShape* m_targetTriangle;

	protected:

		/// Set to true if the two triangle shapes were passed in from the outside.
		uint32_t m_externallyAllocatedTriangles;
	};
}
