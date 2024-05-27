set(SOURCES
	include/F4SE/API.h
	include/F4SE/F4SE.h
	include/F4SE/Impl/PCH.h
	include/F4SE/InputMap.h
	include/F4SE/Interfaces.h
	include/F4SE/Logger.h
	include/F4SE/Trampoline.h
	include/F4SE/Version.h
	include/RE/Bethesda/AITimeStamp.h
	include/RE/Bethesda/ActiveEffect.h
	include/RE/Bethesda/Actor.h
	include/RE/Bethesda/ActorValueInfo.h
	include/RE/Bethesda/Archive2.h
	include/RE/Bethesda/Atomic.h
	include/RE/Bethesda/BGSBaseAliases.h
	include/RE/Bethesda/BGSBodyPartDefs.h
	include/RE/Bethesda/BGSCharacterMorph.h
	include/RE/Bethesda/BGSCharacterTint.h
	include/RE/Bethesda/BGSCreatedObjectManager.h
	include/RE/Bethesda/BGSDefaultObjectManager.h
	include/RE/Bethesda/BGSDynamicPersistenceManager.h
	include/RE/Bethesda/BGSEntryPoint.h
	include/RE/Bethesda/BGSHeadPart.h
	include/RE/Bethesda/BGSInventoryInterface.h
	include/RE/Bethesda/BGSInventoryItem.h
	include/RE/Bethesda/BGSMod.h
	include/RE/Bethesda/BGSPrimitive.h
	include/RE/Bethesda/BGSSaveLoad.h
	include/RE/Bethesda/BGSSceneAction.h
	include/RE/Bethesda/BGSStoryEventManager.h
	include/RE/Bethesda/BGSStoryManagerTreeForm.h
	include/RE/Bethesda/BGSSynchronizedAnimationManager.h
	include/RE/Bethesda/BGSTextureSet.h
	include/RE/Bethesda/BSAnimationGraph.h
	include/RE/Bethesda/BSAttachTechniques.h
	include/RE/Bethesda/BSAudioManager.h
	include/RE/Bethesda/BSAudioUtil.h
	include/RE/Bethesda/BSBTreeFile.h
	include/RE/Bethesda/BSBound.h
	include/RE/Bethesda/BSContainer.h
	include/RE/Bethesda/BSExtraData.h
	include/RE/Bethesda/BSFadeNode.h
	include/RE/Bethesda/BSFixedString.h
	include/RE/Bethesda/BSGeometry.h
	include/RE/Bethesda/BSGraphics.h
	include/RE/Bethesda/BSGraphicsUtility.h
	include/RE/Bethesda/BSHavok.h
	include/RE/Bethesda/BSInputDeviceManager.h
	include/RE/Bethesda/BSInputEnableManager.h
	include/RE/Bethesda/BSInputEventReceiver.h
	include/RE/Bethesda/BSInputEventSingleUser.h
	include/RE/Bethesda/BSInputEventUser.h
	include/RE/Bethesda/BSLock.h
	include/RE/Bethesda/BSMTAManager.h
	include/RE/Bethesda/BSMemStorage.h
	include/RE/Bethesda/BSModelDB.h
	include/RE/Bethesda/BSPointerHandle.h
	include/RE/Bethesda/BSPreCulledObjects.h
	include/RE/Bethesda/BSResource.h
	include/RE/Bethesda/BSResource/AsyncStream.h
	include/RE/Bethesda/BSResource/BSResourceEnums.h
	include/RE/Bethesda/BSResource/Entry.h
	include/RE/Bethesda/BSResource/GlobalLocations.h
	include/RE/Bethesda/BSResource/GlobalPaths.h
	include/RE/Bethesda/BSResource/Location.h
	include/RE/Bethesda/BSResource/LooseFileStreamBase.h
	include/RE/Bethesda/BSResource/Stream.h
	include/RE/Bethesda/BSResource/StreamBase.h
	include/RE/Bethesda/BSResourceNiBinaryStream.h
	include/RE/Bethesda/BSScaleformManager.h
	include/RE/Bethesda/BSScript.h
	include/RE/Bethesda/BSScript/Array.h
	include/RE/Bethesda/BSScript/ArrayWrapper.h
	include/RE/Bethesda/BSScript/CompiledScriptLoader.h
	include/RE/Bethesda/BSScript/ErrorLogger.h
	include/RE/Bethesda/BSScript/ICachedErrorMessage.h
	include/RE/Bethesda/BSScript/IClientVM.h
	include/RE/Bethesda/BSScript/IComplexType.h
	include/RE/Bethesda/BSScript/IFunction.h
	include/RE/Bethesda/BSScript/IHandleReaderWriter.h
	include/RE/Bethesda/BSScript/ILoader.h
	include/RE/Bethesda/BSScript/IMemoryPagePolicy.h
	include/RE/Bethesda/BSScript/IObjectHandlePolicy.h
	include/RE/Bethesda/BSScript/IObjectProcessor.h
	include/RE/Bethesda/BSScript/IProfilePolicy.h
	include/RE/Bethesda/BSScript/ISavePatcherInterface.h
	include/RE/Bethesda/BSScript/IStackCallbackFunctor.h
	include/RE/Bethesda/BSScript/IStackCallbackSaveInterface.h
	include/RE/Bethesda/BSScript/IVMDebugInterface.h
	include/RE/Bethesda/BSScript/IVMObjectBindInterface.h
	include/RE/Bethesda/BSScript/IVMSaveLoadInterface.h
	include/RE/Bethesda/BSScript/IVirtualMachine.h
	include/RE/Bethesda/BSScript/Internal/AttachedScript.h
	include/RE/Bethesda/BSScript/Internal/CodeTasklet.h
	include/RE/Bethesda/BSScript/Internal/EventRelay.h
	include/RE/Bethesda/BSScript/Internal/FunctionMessage.h
	include/RE/Bethesda/BSScript/Internal/IFuncCallQuery.h
	include/RE/Bethesda/BSScript/Internal/RawFuncCallQuery.h
	include/RE/Bethesda/BSScript/Internal/ReadableStringTable.h
	include/RE/Bethesda/BSScript/Internal/ReadableTypeTable.h
	include/RE/Bethesda/BSScript/Internal/ScriptFunction.h
	include/RE/Bethesda/BSScript/Internal/SuspendedStack.h
	include/RE/Bethesda/BSScript/Internal/VDescTable.h
	include/RE/Bethesda/BSScript/Internal/VirtualMachine.h
	include/RE/Bethesda/BSScript/Internal/WritableStringTable.h
	include/RE/Bethesda/BSScript/Internal/WritableTypeTable.h
	include/RE/Bethesda/BSScript/LinkerProcessor.h
	include/RE/Bethesda/BSScript/LogEvent.h
	include/RE/Bethesda/BSScript/MergedBoundScript.h
	include/RE/Bethesda/BSScript/NF_util/NativeFunctionBase.h
	include/RE/Bethesda/BSScript/Object.h
	include/RE/Bethesda/BSScript/ObjectBindPolicy.h
	include/RE/Bethesda/BSScript/ObjectTypeInfo.h
	include/RE/Bethesda/BSScript/PackedInstructionStream.h
	include/RE/Bethesda/BSScript/PropertyGroupInfo.h
	include/RE/Bethesda/BSScript/PropertyTypeInfo.h
	include/RE/Bethesda/BSScript/SimpleAllocMemoryPagePolicy.h
	include/RE/Bethesda/BSScript/Stack.h
	include/RE/Bethesda/BSScript/StackFrame.h
	include/RE/Bethesda/BSScript/StatsEvent.h
	include/RE/Bethesda/BSScript/Struct.h
	include/RE/Bethesda/BSScript/StructTypeInfo.h
	include/RE/Bethesda/BSScript/TypeInfo.h
	include/RE/Bethesda/BSScript/Variable.h
	include/RE/Bethesda/BSScriptUtil.h
	include/RE/Bethesda/BSSemaphore.h
	include/RE/Bethesda/BSShader.h
	include/RE/Bethesda/BSShaderMaterial.h
	include/RE/Bethesda/BSShaderProperty.h
	include/RE/Bethesda/BSSoundHandle.h
	include/RE/Bethesda/BSSpring.h
	include/RE/Bethesda/BSStorage.h
	include/RE/Bethesda/BSStringPool.h
	include/RE/Bethesda/BSStringT.h
	include/RE/Bethesda/BSSystemFile.h
	include/RE/Bethesda/BSSystemFileStreamer.h
	include/RE/Bethesda/BSSystemUtility.h
	include/RE/Bethesda/BSTArray.h
	include/RE/Bethesda/BSTBTree.h
	include/RE/Bethesda/BSTEvent.h
	include/RE/Bethesda/BSTFreeList.h
	include/RE/Bethesda/BSTHashMap.h
	include/RE/Bethesda/BSTInterpolator.h
	include/RE/Bethesda/BSTList.h
	include/RE/Bethesda/BSTMessageQueue.h
	include/RE/Bethesda/BSTObjectArena.h
	include/RE/Bethesda/BSTOptional.h
	include/RE/Bethesda/BSTPoint.h
	include/RE/Bethesda/BSTPointerAndFlags.h
	include/RE/Bethesda/BSTSingleton.h
	include/RE/Bethesda/BSTSmallIndexScatterTable.h
	include/RE/Bethesda/BSTSmartPointer.h
	include/RE/Bethesda/BSTTuple.h
	include/RE/Bethesda/BSTempEffect.h
	include/RE/Bethesda/BSTextureDB.h
	include/RE/Bethesda/BSTextureSet.h
	include/RE/Bethesda/BSTextureStreamer.h
	include/RE/Bethesda/BSThread.h
	include/RE/Bethesda/BSTimer.h
	include/RE/Bethesda/BSUtilities.h
	include/RE/Bethesda/BSVisit.h
	include/RE/Bethesda/CELLJobs.h
	include/RE/Bethesda/CRC.h
	include/RE/Bethesda/Calendar.h
	include/RE/Bethesda/CombatFormulas.h
	include/RE/Bethesda/Console.h
	include/RE/Bethesda/ControlMap.h
	include/RE/Bethesda/Events.h
	include/RE/Bethesda/FavoritesManager.h
	include/RE/Bethesda/FormComponents.h
	include/RE/Bethesda/FormFactory.h
	include/RE/Bethesda/FormUtil.h
	include/RE/Bethesda/GamePlayFormulas.h
	include/RE/Bethesda/GameScript.h
	include/RE/Bethesda/HUDModes.h
	include/RE/Bethesda/IMenu.h
	include/RE/Bethesda/IMovementInterface.h
	include/RE/Bethesda/ImageSpaceData.h
	include/RE/Bethesda/ImageSpaceEffect.h
	include/RE/Bethesda/ImageSpaceManager.h
	include/RE/Bethesda/ImageSpaceModifier.h
	include/RE/Bethesda/InputDevice.h
	include/RE/Bethesda/InputEvent.h
	include/RE/Bethesda/Interface3D.h
	include/RE/Bethesda/InventoryUserUIUtils.h
	include/RE/Bethesda/ItemCrafted.h
	include/RE/Bethesda/MagicItems.h
	include/RE/Bethesda/Main.h
	include/RE/Bethesda/MemoryManager.h
	include/RE/Bethesda/MenuControls.h
	include/RE/Bethesda/MenuCursor.h
	include/RE/Bethesda/MenuTopicManager.h
	include/RE/Bethesda/MessageMenuManager.h
	include/RE/Bethesda/Movement.h
	include/RE/Bethesda/NavMesh.h
	include/RE/Bethesda/NavMeshInfoMap.h
	include/RE/Bethesda/PipboyDataGroup.h
	include/RE/Bethesda/PipboyInventoryUtils.h
	include/RE/Bethesda/PipboyManager.h
	include/RE/Bethesda/PipboyValue.h
	include/RE/Bethesda/PlayerCharacter.h
	include/RE/Bethesda/PlayerControls.h
	include/RE/Bethesda/PowerArmor.h
	include/RE/Bethesda/PowerUtils.h
	include/RE/Bethesda/ProcessLists.h
	include/RE/Bethesda/Projectiles.h
	include/RE/Bethesda/ReferenceEffectController.h
	include/RE/Bethesda/SCRIPT_OUTPUT.h
	include/RE/Bethesda/SDirectory2.h
	include/RE/Bethesda/SWFToCodeFunctionHandler.h
	include/RE/Bethesda/Script.h
	include/RE/Bethesda/SendHUDMessage.h
	include/RE/Bethesda/Settings.h
	include/RE/Bethesda/Sky.h
	include/RE/Bethesda/SplineUtils.h
	include/RE/Bethesda/TESBoundAnimObjects.h
	include/RE/Bethesda/TESBoundObjects.h
	include/RE/Bethesda/TESCamera.h
	include/RE/Bethesda/TESCombatStyle.h
	include/RE/Bethesda/TESCondition.h
	include/RE/Bethesda/TESDataHandler.h
	include/RE/Bethesda/TESFaction.h
	include/RE/Bethesda/TESFile.h
	include/RE/Bethesda/TESForms.h
	include/RE/Bethesda/TESObjectREFRs.h
	include/RE/Bethesda/TESPackages.h
	include/RE/Bethesda/TESRace.h
	include/RE/Bethesda/TESWaterForm.h
	include/RE/Bethesda/TESWorldSpace.h
	include/RE/Bethesda/TaskQueueInterface.h
	include/RE/Bethesda/UI.h
	include/RE/Bethesda/UIMessage.h
	include/RE/Bethesda/UIMessageQueue.h
	include/RE/Bethesda/UIShaderFXInfo.h
	include/RE/Bethesda/UserEvents.h
	include/RE/Bethesda/Utilities.h
	include/RE/Bethesda/VATS.h
	include/RE/Bethesda/Workshop.h
	include/RE/Bethesda/bhkCharacterController.h
	include/RE/Bethesda/bhkPickData.h
	include/RE/Fallout.h
	include/RE/Havok/hkArray.h
	include/RE/Havok/hkBaseObject.h
	include/RE/Havok/hkBaseTypes.h
	include/RE/Havok/hkBlockStream.h
	include/RE/Havok/hkHandle.h
	include/RE/Havok/hkLifoAllocator.h
	include/RE/Havok/hkMemoryAllocator.h
	include/RE/Havok/hkMemoryRouter.h
	include/RE/Havok/hkMemorySystem.h
	include/RE/Havok/hkRefPtr.h
	include/RE/Havok/hkReferencedObject.h
	include/RE/Havok/hkSimdFloat.h
	include/RE/Havok/hkVector4.h
	include/RE/Havok/hknpAllHitsCollector.h
	include/RE/Havok/hknpBodyId.h
	include/RE/Havok/hknpCharacterContext.h
	include/RE/Havok/hknpCharacterState.h
	include/RE/Havok/hknpCharacterSurfaceInfo.h
	include/RE/Havok/hknpClosestHitCollector.h
	include/RE/Havok/hknpClosestUniqueBodyIdHitCollector.h
	include/RE/Havok/hknpCollisionQueryCollector.h
	include/RE/Havok/hknpCollisionResult.h
	include/RE/Havok/hknpMaterialId.h
	include/RE/Havok/hknpShape.h
	include/RE/Havok/hknpUniqueBodyIdHitCollector.h
	include/RE/NetImmerse/NiAVObject.h
	include/RE/NetImmerse/NiAlphaProperty.h
	include/RE/NetImmerse/NiBinaryStream.h
	include/RE/NetImmerse/NiBound.h
	include/RE/NetImmerse/NiCamera.h
	include/RE/NetImmerse/NiCloningProcess.h
	include/RE/NetImmerse/NiCollisionObject.h
	include/RE/NetImmerse/NiColor.h
	include/RE/NetImmerse/NiController.h
	include/RE/NetImmerse/NiExtraData.h
	include/RE/NetImmerse/NiFile.h
	include/RE/NetImmerse/NiFlags.h
	include/RE/NetImmerse/NiFrustum.h
	include/RE/NetImmerse/NiMatrix3.h
	include/RE/NetImmerse/NiMemStream.h
	include/RE/NetImmerse/NiNode.h
	include/RE/NetImmerse/NiObject.h
	include/RE/NetImmerse/NiObjectNET.h
	include/RE/NetImmerse/NiPoint2.h
	include/RE/NetImmerse/NiPoint3.h
	include/RE/NetImmerse/NiPoint4.h
	include/RE/NetImmerse/NiProperty.h
	include/RE/NetImmerse/NiQuaternion.h
	include/RE/NetImmerse/NiRTTI.h
	include/RE/NetImmerse/NiRect.h
	include/RE/NetImmerse/NiRefObject.h
	include/RE/NetImmerse/NiShadeProperty.h
	include/RE/NetImmerse/NiSmartPointer.h
	include/RE/NetImmerse/NiStream.h
	include/RE/NetImmerse/NiStringExtraData.h
	include/RE/NetImmerse/NiTArray.h
	include/RE/NetImmerse/NiTCollection.h
	include/RE/NetImmerse/NiTDefaultAllocator.h
	include/RE/NetImmerse/NiTLargeArray.h
	include/RE/NetImmerse/NiTList.h
	include/RE/NetImmerse/NiTListBase.h
	include/RE/NetImmerse/NiTMap.h
	include/RE/NetImmerse/NiTMapBase.h
	include/RE/NetImmerse/NiTPointerAllocator.h
	include/RE/NetImmerse/NiTPointerListBase.h
	include/RE/NetImmerse/NiTPointerMap.h
	include/RE/NetImmerse/NiTexture.h
	include/RE/NetImmerse/NiTimeController.h
	include/RE/NetImmerse/NiTransform.h
	include/RE/NetImmerse/NiUpdateData.h
	include/RE/NiRTTI_IDs.h
	include/RE/RTTI.h
	include/RE/RTTI_IDs.h
	include/RE/Scaleform/GFx/GFx_AS3.h
	include/RE/Scaleform/GFx/GFx_ASMovieRootBase.h
	include/RE/Scaleform/GFx/GFx_ASString.h
	include/RE/Scaleform/GFx/GFx_Loader.h
	include/RE/Scaleform/GFx/GFx_Log.h
	include/RE/Scaleform/GFx/GFx_Player.h
	include/RE/Scaleform/GFx/GFx_PlayerImpl.h
	include/RE/Scaleform/GFx/GFx_PlayerStats.h
	include/RE/Scaleform/GFx/GFx_Resource.h
	include/RE/Scaleform/GFx/GFx_Stats.h
	include/RE/Scaleform/GFx/GFx_Types.h
	include/RE/Scaleform/Kernel/SF_AllocInfo.h
	include/RE/Scaleform/Kernel/SF_Allocator.h
	include/RE/Scaleform/Kernel/SF_Array.h
	include/RE/Scaleform/Kernel/SF_ArrayPaged.h
	include/RE/Scaleform/Kernel/SF_Atomic.h
	include/RE/Scaleform/Kernel/SF_List.h
	include/RE/Scaleform/Kernel/SF_Log.h
	include/RE/Scaleform/Kernel/SF_Memory.h
	include/RE/Scaleform/Kernel/SF_MemoryHeap.h
	include/RE/Scaleform/Kernel/SF_RefCount.h
	include/RE/Scaleform/Kernel/SF_Stats.h
	include/RE/Scaleform/Kernel/SF_SysAlloc.h
	include/RE/Scaleform/Kernel/SF_System.h
	include/RE/Scaleform/Kernel/SF_Threads.h
	include/RE/Scaleform/Kernel/SF_Types.h
	include/RE/Scaleform/Render/Render_Color.h
	include/RE/Scaleform/Render/Render_Constants.h
	include/RE/Scaleform/Render/Render_Containers.h
	include/RE/Scaleform/Render/Render_Context.h
	include/RE/Scaleform/Render/Render_Matrix2x4.h
	include/RE/Scaleform/Render/Render_Matrix3x4.h
	include/RE/Scaleform/Render/Render_Matrix4x4.h
	include/RE/Scaleform/Render/Render_ScreenToWorld.h
	include/RE/Scaleform/Render/Render_ThreadCommandQueue.h
	include/RE/Scaleform/Render/Render_TreeNode.h
	include/RE/Scaleform/Render/Render_TreeShape.h
	include/RE/Scaleform/Render/Render_Types2D.h
	include/RE/Scaleform/Render/Render_Viewport.h
	include/RE/VTABLE_IDs.h
	include/RE/msvc/memory.h
	include/RE/msvc/typeinfo.h
	include/REL/IAT.h
	include/REL/ID.h
	include/REL/IDDB.h
	include/REL/Module.h
	include/REL/Offset.h
	include/REL/Offset2ID.h
	include/REL/REL.h
	include/REL/Relocation.h
	include/REL/Segment.h
	include/REL/Version.h
	include/REX/PS4.h
	include/REX/PS4/SCEPAD.h
	include/REX/W32.h
	include/REX/W32/ADVAPI32.h
	include/REX/W32/BASE.h
	include/REX/W32/BCRYPT.h
	include/REX/W32/COM.h
	include/REX/W32/COMPTR.h
	include/REX/W32/D3D.h
	include/REX/W32/D3D11.h
	include/REX/W32/D3D11_1.h
	include/REX/W32/D3D11_2.h
	include/REX/W32/D3D11_3.h
	include/REX/W32/D3D11_4.h
	include/REX/W32/D3DCOMPILER.h
	include/REX/W32/DBGHELP.h
	include/REX/W32/DINPUT.h
	include/REX/W32/DXGI.h
	include/REX/W32/DXGI_2.h
	include/REX/W32/DXGI_3.h
	include/REX/W32/DXGI_4.h
	include/REX/W32/DXGI_5.h
	include/REX/W32/DXGI_6.h
	include/REX/W32/KERNEL32.h
	include/REX/W32/OLE32.h
	include/REX/W32/SHELL32.h
	include/REX/W32/USER32.h
	include/REX/W32/VERSION.h
	include/REX/W32/XINPUT.h
	src/F4SE/API.cpp
	src/F4SE/Impl/PCH.cpp
	src/F4SE/InputMap.cpp
	src/F4SE/Interfaces.cpp
	src/F4SE/Logger.cpp
	src/F4SE/Trampoline.cpp
	src/RE/Bethesda/Actor.cpp
	src/RE/Bethesda/BGSInventoryItem.cpp
	src/RE/Bethesda/BSExtraData.cpp
	src/RE/Bethesda/BSResource.cpp
	src/RE/Bethesda/BSResource/Stream.cpp
	src/RE/Bethesda/BSResource/StreamBase.cpp
	src/RE/Bethesda/BSResourceNiBinaryStream.cpp
	src/RE/Bethesda/BSScaleformManager.cpp
	src/RE/Bethesda/BSScript.cpp
	src/RE/Bethesda/BSScript/Array.cpp
	src/RE/Bethesda/BSScript/Internal/VirtualMachine.cpp
	src/RE/Bethesda/BSScript/Object.cpp
	src/RE/Bethesda/BSScript/ObjectTypeInfo.cpp
	src/RE/Bethesda/BSScript/PackedInstructionStream.cpp
	src/RE/Bethesda/BSScript/StackFrame.cpp
	src/RE/Bethesda/BSScript/Struct.cpp
	src/RE/Bethesda/BSScript/StructTypeInfo.cpp
	src/RE/Bethesda/BSScript/TypeInfo.cpp
	src/RE/Bethesda/BSScript/Variable.cpp
	src/RE/Bethesda/BSVisit.cpp
	src/RE/Bethesda/CRC.cpp
	src/RE/Bethesda/Calendar.cpp
	src/RE/Bethesda/FormComponents.cpp
	src/RE/Bethesda/IMenu.cpp
	src/RE/Bethesda/MenuCursor.cpp
	src/RE/Bethesda/PlayerCharacter.cpp
	src/RE/Bethesda/TESBoundAnimObjects.cpp
	src/RE/Bethesda/TESForms.cpp
	src/RE/Bethesda/TESObjectREFRs.cpp
	src/RE/Fallout.cpp
	src/RE/NetImmerse/NiAVObject.cpp
	src/RE/NetImmerse/NiBinaryStream.cpp
	src/RE/NetImmerse/NiObjectNET.cpp
	src/RE/NetImmerse/NiPoint3.cpp
	src/RE/NetImmerse/NiRect.cpp
	src/RE/Scaleform/GFx/GFx_Player.cpp
	src/REL/IAT.cpp
	src/REL/IDDB.cpp
	src/REL/Module.cpp
	src/REL/Relocation.cpp
	src/REL/Version.cpp
	src/REX/W32/ADVAPI32.cpp
	src/REX/W32/BCRYPT.cpp
	src/REX/W32/D3D11.cpp
	src/REX/W32/D3DCOMPILER.cpp
	src/REX/W32/DBGHELP.cpp
	src/REX/W32/DXGI.cpp
	src/REX/W32/KERNEL32.cpp
	src/REX/W32/OLE32.cpp
	src/REX/W32/SHELL32.cpp
	src/REX/W32/USER32.cpp
	src/REX/W32/VERSION.cpp
)
