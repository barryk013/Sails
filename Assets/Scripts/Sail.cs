using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;

[RequireComponent(typeof(Animator))]
public class Sail : MonoBehaviour
{
    [SerializeField] private float sailAnimTime = 5;
    [SerializeField] private Vector3 forwardDirection = new(0, 0, 1);
    [SerializeField] private Renderer sailRenderer;
    private Material _sailMaterial;

    private bool _sailDeployed = false;
    private float _currentFillPercent = 0;
    /// <summary>
    /// 1 = stowed sail (rolled up),
    /// 0 = doused sail (rolled down, dropped)
    /// </summary>
    private float _currentRollUpPercent = 1;

    private Animator _animator;

    private Coroutine _sailDeployCoroutine;
    private Coroutine _sailWindCoroutine;
    private Coroutine _resetCoroutine;

    [SerializeField] private WindDataScriptableObject windData;

    private void Awake()
    {
        _animator = GetComponent<Animator>();
        _sailMaterial = sailRenderer.material;

        _sailMaterial.SetVector("_ForwardDirection", forwardDirection.normalized);
        //Voronoi seed randomized so each sail moves in a slightly different way.
        _sailMaterial.SetFloat("_VoronoiSeed", UnityEngine.Random.Range(0, 10000));

        _animator.Play("RollUp", 0, 1);
    }
    private void Update()
    {
        // _sailMaterial.SetVector("_ForwardDirection", forwardDirection.normalized);
    }

    #region Event Listeners    
    public void OnSailCommandIssued()
    {
        if (_sailDeployed)
        {
            StopCoroutines();
            _sailDeployed = false;
            _resetCoroutine = StartCoroutine(ResetSailCoroutine());
        }
        else
        {
            StopCoroutines();
            _sailDeployed = true;
            _sailDeployCoroutine = StartCoroutine(SailDeployCoroutine());
            _sailWindCoroutine = StartCoroutine(SailWindCoroutine());
        }
    }
    #endregion

    #region Coroutines

    private void StopCoroutines()
    {
        if (_sailDeployCoroutine != null)
            StopCoroutine(_sailDeployCoroutine);

        if (_sailWindCoroutine != null)
            StopCoroutine(_sailWindCoroutine);

        if (_resetCoroutine != null)
            StopCoroutine(_resetCoroutine);
    }

    /// <summary>
    /// Deploy (roll down) sail coroutine.
    /// </summary>
    /// <returns></returns>
    IEnumerator SailDeployCoroutine()
    {
        float fromPercent = _currentRollUpPercent;

        float lerpDuration = fromPercent * sailAnimTime;
        float lerpTimer = 0;

        while (lerpTimer < lerpDuration)
        {
            _currentRollUpPercent = Mathf.Lerp(fromPercent, 0, lerpTimer / lerpDuration);
            _animator.Play("RollUp", 0, _currentRollUpPercent);

            lerpTimer += Time.deltaTime;

            yield return null;
        }
        _sailDeployCoroutine = null;
    }

    /// <summary>
    /// Fill sails depending on wind direction.
    /// </summary>
    /// <returns></returns>
    IEnumerator SailWindCoroutine()
    {
        while (_sailDeployed)
        {
            float sailToWindAngle = Vector3.Angle(transform.forward, windData.WindDirection);
            //wind at 90deg to sail = 0 fill percent. 
            //0 to 180 range -> -90 to 90 range -> flip sign -> divide by 90 -> sail fill percent
            float sailFillPercent = (-sailToWindAngle + 90) / 90;  


            if (_currentFillPercent < sailFillPercent)
            {
                _currentFillPercent += Time.deltaTime * (1 / sailAnimTime);
            }
            else if (_currentFillPercent > sailFillPercent)
            {
                _currentFillPercent -= Time.deltaTime * (1 / sailAnimTime);
            }

            
            _currentFillPercent = Mathf.Clamp(_currentFillPercent, -1, 1);

            if (_currentFillPercent > 0)
                _animator.Play("PositiveWind", 1, _currentFillPercent);
            else
                _animator.Play("NegativeWind", 1, Mathf.Abs(_currentFillPercent));

            yield return null;
        }
    }

    IEnumerator ResetSailCoroutine()
    {
        float startingRollDownPercent = _currentRollUpPercent;
        float startingFillPercent = _currentFillPercent;

        float lerpDuration = (1 - _currentRollUpPercent) * sailAnimTime;
        float lerpTimer = 0;

        while (lerpTimer < lerpDuration)
        {
            _currentRollUpPercent = Mathf.Lerp(startingRollDownPercent, 1, lerpTimer / lerpDuration);
            _currentFillPercent = Mathf.Lerp(startingFillPercent, 0, lerpTimer / lerpDuration);
            
            //update animations
            _animator.Play("RollUp", 0, _currentRollUpPercent);

            if (_currentFillPercent > 0)
                _animator.Play("PositiveWind", 1, _currentFillPercent);
            else
                _animator.Play("NegativeWind", 1, Mathf.Abs(_currentFillPercent));

            lerpTimer += Time.deltaTime;

            yield return null;
        }
        _sailDeployCoroutine = null;
        _sailWindCoroutine = null;
    }

    #endregion
}
