cmake_minimum_required(VERSION 3.21)
include("cmake/common.cmake")
option(F4SE_SUPPORT_XBYAK "Enables trampoline support for Xbyak." ON)

project(umbrella)

if("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
	message(FATAL_ERROR "in-source builds are not allowed")
endif()

function(conditionally_add_subdirectory SRC_DIR)
	if(NOT TARGET "${SUBDIRECTORY}")
		if("${ARGC}" GREATER 1)
			add_subdirectory("${SRC_DIR}" "${ARGV1}")
		else()
			add_subdirectory("${SRC_DIR}")
		endif()
	endif()
endfunction()

include(CTest)

list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

conditionally_add_subdirectory(CommonLibF4)
conditionally_add_subdirectory("../F4 Projects/CompanionCombatDistance" "CCD")
conditionally_add_subdirectory("../F4 Projects/EscapeFreeze" "EscapeFreeze")
conditionally_add_subdirectory("../F4 Projects/NanakoSurgeon" "NanakoSurgeon")
conditionally_add_subdirectory("../F4 Projects/AVF" "AVF")
conditionally_add_subdirectory("../F4 Projects/GrapplingHook" "GrapplingHook")
conditionally_add_subdirectory("../F4 Projects/UneducatedShooter" "UneducatedShooter")
conditionally_add_subdirectory("../F4 Projects/CasingFramework" "CasingFramework")
conditionally_add_subdirectory("../F4 Projects/AutoBeam" "AutoBeam")
conditionally_add_subdirectory("../F4 Projects/ScaleformCopyPaste" "ScaleformCopyPaste")
conditionally_add_subdirectory("../F4 Projects/PUBGAiming" "PUBGAiming")
conditionally_add_subdirectory("../F4 Projects/ImmersiveDamage" "ImmersiveDamage")
conditionally_add_subdirectory("../F4 Projects/ShieldFramework" "ShieldFramework")
conditionally_add_subdirectory("../F4 Projects/DeBadWeapons" "DeBadWeapons")
conditionally_add_subdirectory("../F4 Projects/DamageSystem" "DamageSystem")

include(FetchContent)
FetchContent_Declare(json
  GIT_REPOSITORY https://github.com/ArthurSonzogni/nlohmann_json_cmake_fetchcontent
  GIT_TAG v3.9.1)

FetchContent_GetProperties(json)
if(NOT json_POPULATED)
  FetchContent_Populate(json)
  add_subdirectory(${json_SOURCE_DIR} ${json_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()
