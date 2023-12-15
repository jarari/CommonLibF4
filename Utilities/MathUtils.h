#pragma once
#include <cmath>
#include "RE/NetImmerse/NiMatrix3.h"
#include "RE/NetImmerse/NiPoint3.h"

#ifndef MATH_PI
#define MATH_PI 3.14159265358979323846
#endif
using RE::NiMatrix3;
using RE::NiPoint3;

const static float toRad = (float)(MATH_PI / 180.0);
struct Quaternion {
public:
	float x, y, z, w;
	Quaternion(float _x, float _y, float _z, float _w);
	float Norm();
	NiMatrix3 ToRotationMatrix33();
};

Quaternion::Quaternion(float _x, float _y, float _z, float _w) {
	x = _x;
	y = _y;
	z = _z;
	w = _w;
}

float Quaternion::Norm() {
	return x * x + y * y + z * z + w * w;
}

void SetMatrix33(float a, float b, float c, float d, float e, float f, float g, float h, float i, NiMatrix3& mat) {
	mat.entry[0].pt[0] = a;
	mat.entry[0].pt[1] = b;
	mat.entry[0].pt[2] = c;
	mat.entry[1].pt[0] = d;
	mat.entry[1].pt[1] = e;
	mat.entry[1].pt[2] = f;
	mat.entry[2].pt[0] = g;
	mat.entry[2].pt[1] = h;
	mat.entry[2].pt[2] = i;
}

NiMatrix3 GetRotationMatrix33(float pitch, float yaw, float roll) {
	NiMatrix3 m_roll;
	SetMatrix33(cos(roll), -sin(roll), 0,
				sin(roll), cos(roll), 0,
				0, 0, 1,
				m_roll);
	NiMatrix3 m_yaw;
	SetMatrix33(1, 0, 0,
				0, cos(yaw), -sin(yaw),
				0, sin(yaw), cos(yaw),
				m_yaw);
	NiMatrix3 m_pitch;
	SetMatrix33(cos(pitch), 0, sin(pitch),
				0, 1, 0,
				-sin(pitch), 0, cos(pitch),
				m_pitch);
	return m_roll * m_pitch * m_yaw;
}

NiMatrix3 GetRotationMatrix33(NiPoint3 axis, float angle) {
	float x = axis.x * sin(angle / 2.0f);
	float y = axis.y * sin(angle / 2.0f);
	float z = axis.z * sin(angle / 2.0f);
	float w = cos(angle / 2.0f);
	Quaternion q = Quaternion(x, y, z, w);
	return q.ToRotationMatrix33();
}

NiMatrix3 GetScaleMatrix(float x, float y, float z) {
	NiMatrix3 ret;
	SetMatrix33(x, 0, 0, 0, y, 0, 0, 0, z, ret);
	return ret;
}

//From https://android.googlesource.com/platform/external/jmonkeyengine/+/59b2e6871c65f58fdad78cd7229c292f6a177578/engine/src/core/com/jme3/math/Quaternion.java
NiMatrix3 Quaternion::ToRotationMatrix33() {
	float norm = Norm();
	// we explicitly test norm against one here, saving a division
	// at the cost of a test and branch.  Is it worth it?
	float s = (norm == 1.0f) ? 2.0f : (norm > 0.0f) ? 2.0f / norm : 0;

	// compute xs/ys/zs first to save 6 multiplications, since xs/ys/zs
	// will be used 2-4 times each.
	float xs = x * s;
	float ys = y * s;
	float zs = z * s;
	float xx = x * xs;
	float xy = x * ys;
	float xz = x * zs;
	float xw = w * xs;
	float yy = y * ys;
	float yz = y * zs;
	float yw = w * ys;
	float zz = z * zs;
	float zw = w * zs;

	// using s=2/norm (instead of 1/norm) saves 9 multiplications by 2 here
	NiMatrix3 mat;
	SetMatrix33(1 - (yy + zz), (xy - zw), (xz + yw),
				(xy + zw), 1 - (xx + zz), (yz - xw),
				(xz - yw), (yz + xw), 1 - (xx + yy),
				mat);

	return mat;
}

//Sarrus rule
float Determinant(NiMatrix3 mat) {
	return mat.entry[0].pt[0] * mat.entry[1].pt[1] * mat.entry[2].pt[2]
		+ mat.entry[0].pt[1] * mat.entry[1].pt[2] * mat.entry[2].pt[0]
		+ mat.entry[0].pt[2] * mat.entry[1].pt[0] * mat.entry[2].pt[1]
		- mat.entry[0].pt[2] * mat.entry[1].pt[1] * mat.entry[2].pt[0]
		- mat.entry[0].pt[1] * mat.entry[1].pt[0] * mat.entry[2].pt[2]
		- mat.entry[0].pt[0] * mat.entry[1].pt[2] * mat.entry[2].pt[1];
}

NiMatrix3 Inverse(NiMatrix3 mat) {
	float det = Determinant(mat);
	if (det == 0) {
		NiMatrix3 idmat;
		idmat.MakeIdentity();
		return idmat;
	}
	float a = mat.entry[0].pt[0];
	float b = mat.entry[0].pt[1];
	float c = mat.entry[0].pt[2];
	float d = mat.entry[1].pt[0];
	float e = mat.entry[1].pt[1];
	float f = mat.entry[1].pt[2];
	float g = mat.entry[2].pt[0];
	float h = mat.entry[2].pt[1];
	float i = mat.entry[2].pt[2];
	NiMatrix3 invmat;
	SetMatrix33(e * i - f * h, -(b * i - c * h), b * f - c * e,
				-(d * i - f * g), a * i - c * g, -(a * f - c * d),
				d * h - e * g, -(a * h - b * g), a * e - b * d,
				invmat);
	return invmat * (1.0f / det);
}

NiMatrix3 Transpose(NiMatrix3 mat) {
	NiMatrix3 trans;
	float a = mat.entry[0].pt[0];
	float b = mat.entry[0].pt[1];
	float c = mat.entry[0].pt[2];
	float d = mat.entry[1].pt[0];
	float e = mat.entry[1].pt[1];
	float f = mat.entry[1].pt[2];
	float g = mat.entry[2].pt[0];
	float h = mat.entry[2].pt[1];
	float i = mat.entry[2].pt[2];
	SetMatrix33(a, d, g,
				b, e, h,
				c, f, i, trans);
	return trans;
}

NiPoint3 ToDirectionVector(NiMatrix3 mat) {
	return NiPoint3(mat.entry[2].pt[0], mat.entry[2].pt[1], mat.entry[2].pt[2]);
}

NiPoint3 ToUpVector(NiMatrix3 mat) {
	return NiPoint3(mat.entry[1].pt[0], mat.entry[1].pt[1], mat.entry[1].pt[2]);
}

NiPoint3 ToRightVector(NiMatrix3 mat) {
	return NiPoint3(mat.entry[0].pt[0], mat.entry[0].pt[1], mat.entry[0].pt[2]);
}

//(Rotation Matrix)^-1 * (World pos - Local Origin)
NiPoint3 WorldToLocal(NiPoint3 wpos, NiPoint3 lorigin, NiMatrix3 rot) {
	NiPoint3 lpos = wpos - lorigin;
	NiMatrix3 invrot = Transpose(rot);
	return invrot * lpos;
}

NiPoint3 LocalToWorld(NiPoint3 lpos, NiPoint3 lorigin, NiMatrix3 rot) {
	return rot * lpos + lorigin;
}

NiPoint3 CrossProduct(NiPoint3 a, NiPoint3 b) {
	NiPoint3 ret;
	ret[0] = a[1] * b[2] - a[2] * b[1];
	ret[1] = a[2] * b[0] - a[0] * b[2];
	ret[2] = a[0] * b[1] - a[1] * b[0];
	if (ret[0] == 0 && ret[1] == 0 && ret[2] == 0) {
		if (a[2] != 0) {
			ret[0] = 1;
		}
		else {
			ret[1] = 1;
		}
	}
	return ret;
}

float DotProduct(NiPoint3 a, NiPoint3 b) {
	return a.x * b.x + a.y * b.y + a.z * b.z;
}

float Length(NiPoint3 p) {
	return sqrt(p.x * p.x + p.y * p.y + p.z * p.z);
}

NiPoint3 Normalize(NiPoint3 p) {
	NiPoint3 ret = p;
	float l = Length(p);
	if (l == 0) {
		ret.x = 1;
		ret.y = 0;
		ret.z = 0;
	}
	else {
		ret.x /= l;
		ret.y /= l;
		ret.z /= l;
	}
	return ret;
}

float easeInOutCubic(float t, float b, float c, float d) {
	if ((t /= d / 2) < 1)
		return c / 2 * t * t * t + b;
	return c / 2 * ((t -= 2) * t * t + 2) + b;
}

float easeInOutQuad(float t) {
	return t < 0.5 ? 2 * t * t : t * (4 - 2 * t) - 1;
}

float easeOutCubic(float t)
{
	return 1 + (--t) * t * t;
}
