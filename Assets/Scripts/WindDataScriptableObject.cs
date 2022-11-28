using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName ="RuntimeData/WindData")]
public class WindDataScriptableObject : ScriptableObject
{
    public Vector3 WindDirection { get => Quaternion.Euler(0, Angle, 0) * Vector3.forward; }
    public float Angle = 0; 
}
